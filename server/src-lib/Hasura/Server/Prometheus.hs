-- | Mutable references for Prometheus metrics.
--
-- These metrics are independent from the metrics in "Hasura.Server.Metrics".
module Hasura.Server.Prometheus
  ( PrometheusMetrics (..),
    GraphQLRequestMetrics (..),
    EventTriggerMetrics (..),
    makeDummyPrometheusMetrics,
    ConnectionsGauge,
    Connections (..),
    newConnectionsGauge,
    readConnectionsGauge,
    incWarpThreads,
    decWarpThreads,
    incWebsocketConnections,
    decWebsocketConnections,
    exportPrometheusMetrics,
  )
where

import Data.ByteString.Builder qualified as B
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Data.List (intersperse)
import Data.Map.Strict qualified as M
import Hasura.Prelude
import System.Metrics.Prometheus.Counter (Counter)
import System.Metrics.Prometheus.Counter qualified as Counter
import System.Metrics.Prometheus.Gauge (Gauge)
import System.Metrics.Prometheus.Gauge qualified as Gauge
import System.Metrics.Prometheus.Histogram (Histogram)
import System.Metrics.Prometheus.Histogram qualified as Histogram

--------------------------------------------------------------------------------

-- | Mutable references for Prometheus metrics.
data PrometheusMetrics = PrometheusMetrics
  { pmConnections :: ConnectionsGauge,
    pmActiveSubscriptions :: Gauge,
    pmGraphQLRequestMetrics :: GraphQLRequestMetrics,
    pmEventTriggerMetrics :: EventTriggerMetrics,
    pmWebSocketBytesReceived :: Counter,
    pmWebSocketBytesSent :: Counter,
    pmActionBytesReceived :: Counter,
    pmActionBytesSent :: Counter,
    pmScheduledTriggerBytesReceived :: Counter,
    pmScheduledTriggerBytesSent :: Counter
  }

data GraphQLRequestMetrics = GraphQLRequestMetrics
  { gqlRequestsQuerySuccess :: Counter,
    gqlRequestsQueryFailure :: Counter,
    gqlRequestsMutationSuccess :: Counter,
    gqlRequestsMutationFailure :: Counter,
    gqlRequestsUnknownFailure :: Counter,
    gqlExecutionTimeSecondsQuery :: Histogram,
    gqlExecutionTimeSecondsMutation :: Histogram
  }

data EventTriggerMetrics = EventTriggerMetrics
  { eventTriggerHTTPWorkers :: Gauge,
    eventQueueTimeSeconds :: Histogram,
    eventsFetchTimePerBatch :: Histogram,
    eventWebhookProcessingTime :: Histogram,
    eventProcessingTime :: Histogram,
    eventTriggerBytesReceived :: Counter,
    eventTriggerBytesSent :: Counter
  }

-- | Create dummy mutable references without associating them to a metrics
-- store.
makeDummyPrometheusMetrics :: IO PrometheusMetrics
makeDummyPrometheusMetrics = do
  pmConnections <- newConnectionsGauge
  pmActiveSubscriptions <- Gauge.new
  pmGraphQLRequestMetrics <- makeDummyGraphQLRequestMetrics
  pmEventTriggerMetrics <- makeDummyEventTriggerMetrics
  pmWebSocketBytesReceived <- Counter.new
  pmWebSocketBytesSent <- Counter.new
  pmActionBytesReceived <- Counter.new
  pmActionBytesSent <- Counter.new
  pmScheduledTriggerBytesReceived <- Counter.new
  pmScheduledTriggerBytesSent <- Counter.new
  pure PrometheusMetrics {..}

makeDummyGraphQLRequestMetrics :: IO GraphQLRequestMetrics
makeDummyGraphQLRequestMetrics = do
  gqlRequestsQuerySuccess <- Counter.new
  gqlRequestsQueryFailure <- Counter.new
  gqlRequestsMutationSuccess <- Counter.new
  gqlRequestsMutationFailure <- Counter.new
  gqlRequestsUnknownFailure <- Counter.new
  gqlExecutionTimeSecondsQuery <- Histogram.new []
  gqlExecutionTimeSecondsMutation <- Histogram.new []
  pure GraphQLRequestMetrics {..}

makeDummyEventTriggerMetrics :: IO EventTriggerMetrics
makeDummyEventTriggerMetrics = do
  eventTriggerHTTPWorkers <- Gauge.new
  eventQueueTimeSeconds <- Histogram.new []
  eventsFetchTimePerBatch <- Histogram.new []
  eventWebhookProcessingTime <- Histogram.new []
  eventProcessingTime <- Histogram.new []
  eventTriggerBytesReceived <- Counter.new
  eventTriggerBytesSent <- Counter.new
  pure EventTriggerMetrics {..}

--------------------------------------------------------------------------------

-- | A mutable reference for atomically sampling the number of websocket
-- connections and number of threads forked by the warp webserver.
--
-- Because we derive the number of (non-websocket) HTTP connections by the
-- difference of these two metrics, we must sample them simultaneously,
-- otherwise we might report a negative number of HTTP connections.
newtype ConnectionsGauge = ConnectionsGauge (IORef Connections)

data Connections = Connections
  { connWarpThreads :: Int64,
    connWebsockets :: Int64
  }

newConnectionsGauge :: IO ConnectionsGauge
newConnectionsGauge =
  ConnectionsGauge
    <$> newIORef Connections {connWarpThreads = 0, connWebsockets = 0}

readConnectionsGauge :: ConnectionsGauge -> IO Connections
readConnectionsGauge (ConnectionsGauge ref) = readIORef ref

incWarpThreads :: ConnectionsGauge -> IO ()
incWarpThreads =
  modifyConnectionsGauge $ \connections ->
    connections {connWarpThreads = connWarpThreads connections + 1}

decWarpThreads :: ConnectionsGauge -> IO ()
decWarpThreads =
  modifyConnectionsGauge $ \connections ->
    connections {connWarpThreads = connWarpThreads connections - 1}

incWebsocketConnections :: ConnectionsGauge -> IO ()
incWebsocketConnections =
  modifyConnectionsGauge $ \connections ->
    connections {connWebsockets = connWebsockets connections + 1}

decWebsocketConnections :: ConnectionsGauge -> IO ()
decWebsocketConnections =
  modifyConnectionsGauge $ \connections ->
    connections {connWebsockets = connWebsockets connections - 1}

modifyConnectionsGauge ::
  (Connections -> Connections) -> ConnectionsGauge -> IO ()
modifyConnectionsGauge f (ConnectionsGauge ref) =
  atomicModifyIORef' ref $ \connections -> (f connections, ())

--------------------------------------------------------------------------------
-- Prometheus exporter

type Labels = [(String, String)]

class Exportable a where
  sample :: String -> Labels -> a -> IO B.Builder
  typeName :: String
  helpString :: Maybe String
  helpString = Nothing

instance Exportable Counter where
  sample name labels metric = do
    value <- Counter.read metric
    return $ toSampleLine name labels (B.stringUtf8 $ show value)
  typeName = "counter"

instance Exportable Gauge where
  sample name labels metric = do
    value <- Gauge.read metric
    return $ toSampleLine name labels (B.stringUtf8 $ show value)
  typeName = "gauge"

instance Exportable Histogram where
  sample name labels metric = do
    Histogram.HistogramSample {..} <- Histogram.read metric
    return $
      mconcat
        [ let cumulativeBuckets =
                snd $ M.mapAccum cumulativeSum 0 (histBuckets)
                where
                  cumulativeSum !sum_ x = let z = sum_ + x in (z, z)
           in flip foldMap (M.toList cumulativeBuckets) $
                \(upperBound, count) ->
                  toSampleLine
                    (name <> "_bucket")
                    (("le", show upperBound) : labels)
                    (B.stringUtf8 $ show count),
          toSampleLine
            (name <> "_bucket")
            (("le", "+Inf") : labels)
            (B.stringUtf8 $ show histCount),
          toSampleLine
            (name <> "_sum")
            labels
            (B.stringUtf8 $ show histSum),
          toSampleLine
            (name <> "_count")
            labels
            (B.stringUtf8 $ show histCount)
        ]
  typeName = "histogram"

newtype HttpConnectionsGauge = HttpConnectionsGauge ConnectionsGauge

instance Exportable HttpConnectionsGauge where
  sample name labels (HttpConnectionsGauge (ConnectionsGauge ref)) = do
    Connections {..} <- readIORef ref
    return $ toSampleLine name labels (B.stringUtf8 $ show connWarpThreads)
  typeName = "gauge"

newtype WsConnectionsGauge = WsConnectionsGauge ConnectionsGauge

instance Exportable WsConnectionsGauge where
  sample name labels (WsConnectionsGauge (ConnectionsGauge ref)) = do
    Connections {..} <- readIORef ref
    return $ toSampleLine name labels (B.stringUtf8 $ show connWebsockets)
  typeName = "gauge"

exportPrometheusMetrics :: PrometheusMetrics -> IO B.Builder
exportPrometheusMetrics PrometheusMetrics {..} = do
  l <- sequence metricLines
  pure $ mconcat $ intersperse (B.charUtf8 '\n') l
  where
    GraphQLRequestMetrics {..} = pmGraphQLRequestMetrics
    EventTriggerMetrics {..} = pmEventTriggerMetrics
    metricLines =
      [ -- TODO: hasura_http_connections, missing metric
        exportMetric
          "hasura_websocket_connections"
          (WsConnectionsGauge pmConnections)
          [],
        exportMetric
          "hasura_active_subscriptions"
          pmActiveSubscriptions
          [],
        exportMetric
          "hasura_graphql_requests_total"
          gqlRequestsQuerySuccess
          [("operation_type", "query"), ("response_status", "success")],
        exportMetric
          "hasura_graphql_requests_total"
          gqlRequestsQueryFailure
          [("operation_type", "query"), ("response_status", "failed")],
        exportMetric
          "hasura_graphql_requests_total"
          gqlRequestsMutationSuccess
          [("operation_type", "mutation"), ("response_status", "success")],
        exportMetric
          "hasura_graphql_requests_total"
          gqlRequestsMutationFailure
          [("operation_type", "mutation"), ("response_status", "failed")],
        exportMetric
          "hasura_graphql_requests_total"
          gqlRequestsUnknownFailure
          [("operation_type", "unknown"), ("response_status", "failed")],
        exportMetric
          "hasura_graphql_execution_time_seconds"
          gqlExecutionTimeSecondsQuery
          [("operation_type", "query")],
        exportMetric
          "hasura_graphql_execution_time_seconds"
          gqlExecutionTimeSecondsMutation
          [("operation_type", "mutation")],
        exportMetric
          "hasura_event_queue_time_seconds"
          eventQueueTimeSeconds
          [],
        exportMetric
          "hasura_event_fetch_time_per_batch_seconds"
          eventsFetchTimePerBatch
          [],
        exportMetric
          "hasura_event_webhook_processing_time_seconds"
          eventWebhookProcessingTime
          [],
        exportMetric
          "hasura_event_processing_time_seconds"
          eventProcessingTime
          [],
        exportMetric
          "hasura_event_trigger_http_workers"
          eventTriggerHTTPWorkers
          []
          -- NOTE: hasura_postgres_connections doesn't make much sence, since
          -- Hasura creates a pool with only one connection, see
          -- server/src-lib/Hasura/Backends/Postgres/Connection/Connect.hs
      ]

exportMetric ::
  forall a.
  (Exportable a) =>
  String ->
  a ->
  Labels ->
  IO B.Builder
exportMetric name metric labels = do
  sampled <- sample name labels metric
  let helpLine = case helpString @a of
        Nothing -> mempty
        Just s -> B.stringUtf8 $ "# HELP " <> s <> "\n"
  let typeLine = B.stringUtf8 $ "# TYPE " <> name <> " " <> typeName @a <> "\n"
  return $ helpLine <> typeLine <> sampled

toSampleLine :: String -> Labels -> B.Builder -> B.Builder
toSampleLine name labels value =
  B.stringUtf8 name
    <> B.charUtf8 '{'
    <> B.stringUtf8 labelsRaw
    <> B.charUtf8 '}'
    <> B.charUtf8 ' '
    <> value
    <> B.charUtf8 '\n'
  where
    labelsRaw = intercalate "," $ map (\(n, v) -> n ++ "=\"" ++ v ++ "\"") labels
