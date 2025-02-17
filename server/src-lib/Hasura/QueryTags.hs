-- | This module has the various metadata we want to attach to the
-- generated/executed query
module Hasura.QueryTags
  ( Attribute,
    LivequeryMetadata (LivequeryMetadata),
    MutationMetadata (MutationMetadata),
    QueryMetadata (QueryMetadata),
    QueryTags (QTLiveQuery, QTMutation, QTQuery),
    QueryTagsAttributes (_unQueryTagsAttributes),
    QueryTagsComment (..),
    emptyQueryTagsComment,
    encodeQueryTags,

    -- * Exposed for testing
    emptyQueryTagsAttributes,
  )
where

import Data.Text.Extended
import Hasura.GraphQL.Namespace (RootFieldAlias)
import Hasura.GraphQL.ParameterizedQueryHash
import Hasura.Prelude
import Hasura.Server.Types (RequestId (..))
import Language.GraphQL.Draft.Syntax qualified as GQL

-- | Query Tags are SQL comments which are made up of (key=value) pairs.
--
-- These are appended to the SQL statements generated by Hasura for GraphQL
-- operations. This enables the ability to get some application context in the
-- database logs and also use native database monitoring tools (e.g. pganalyze)
-- for better performance analysis.
--
-- The application context(query tags) can be used to detect slow GQL operation and relate
-- them back to the SQL that was generated.
--
-- For eg: SELECT name FROM child /* request_id=487c2ed5-08a4-429a-b0e0-4666a82e3cc6, field_name=child, operation_name=GetChild */
--
-- For more usage information, refer [Query Tags Docs](https://hasura.io/docs/latest/graphql/cloud/query-tags.html)
data QueryTags
  = QTQuery !QueryMetadata
  | QTMutation !MutationMetadata
  | QTLiveQuery !LivequeryMetadata
  deriving (Show)

-- | query-tags as SQL comment which is appended to the prepared SQL statement
newtype QueryTagsComment = QueryTagsComment {_unQueryTagsComment :: Text} deriving (Show, Eq)

type Attribute = (Text, Text)

newtype QueryTagsAttributes = QueryTagsAttributes {_unQueryTagsAttributes :: [Attribute]} deriving (Show, Eq)

emptyQueryTagsAttributes :: QueryTagsAttributes
emptyQueryTagsAttributes = QueryTagsAttributes mempty

emptyQueryTagsComment :: QueryTagsComment
emptyQueryTagsComment = QueryTagsComment mempty

data QueryMetadata = QueryMetadata
  { qmRequestId :: Maybe RequestId,
    qmOperationName :: Maybe GQL.Name,
    qmFieldName :: RootFieldAlias,
    qmParameterizedQueryHash :: ParameterizedQueryHash
  }
  deriving (Show)

data MutationMetadata = MutationMetadata
  { mmRequestId :: Maybe RequestId,
    mmOperationName :: Maybe GQL.Name,
    mmFieldName :: RootFieldAlias,
    mmParameterizedQueryHash :: ParameterizedQueryHash
  }
  deriving (Show)

data LivequeryMetadata = LivequeryMetadata
  { lqmFieldName :: RootFieldAlias,
    lqmParameterizedQueryHash :: ParameterizedQueryHash
  }
  deriving (Show)

encodeQueryTags :: QueryTags -> QueryTagsAttributes
encodeQueryTags = \case
  QTQuery queryMetadata -> QueryTagsAttributes $ encodeQueryMetadata queryMetadata
  QTMutation mutationMetadata -> QueryTagsAttributes $ encodeMutationMetadata mutationMetadata
  QTLiveQuery livequeryMetadata -> QueryTagsAttributes $ encodeLivequeryMetadata livequeryMetadata
  where
    -- TODO: how do we want to encode RootFieldAlias?
    -- Currently uses ToTxt instance, which produces "namespace.fieldname"
    encodeQueryMetadata QueryMetadata {..} =
      maybeToList ((,) "request_id" . unRequestId <$> qmRequestId)
        <> [ ("field_name", toTxt qmFieldName),
             ("parameterized_query_hash", bsToTxt $ unParamQueryHash qmParameterizedQueryHash)
           ]
        <> operationNameAttributes qmOperationName

    encodeMutationMetadata MutationMetadata {..} =
      maybeToList ((,) "request_id" . unRequestId <$> mmRequestId)
        <> [ ("field_name", toTxt mmFieldName),
             ("parameterized_query_hash", bsToTxt $ unParamQueryHash mmParameterizedQueryHash)
           ]
        <> operationNameAttributes mmOperationName

    encodeLivequeryMetadata LivequeryMetadata {..} =
      [ ("field_name", toTxt lqmFieldName),
        ("parameterized_query_hash", bsToTxt $ unParamQueryHash lqmParameterizedQueryHash)
      ]

operationNameAttributes :: Maybe GQL.Name -> [(Text, Text)]
operationNameAttributes = maybe [] (\opName -> [("operation_name", GQL.unName opName)])
