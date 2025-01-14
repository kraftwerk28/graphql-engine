import { CapabilitiesResponse } from '@hasura/dc-api-types';
import { AxiosInstance } from 'axios';
import { runMetadataQuery } from '../../api';

export const getDriverCapabilities = async (
  httpClient: AxiosInstance,
  driver?: string
) => {
  if (!driver) throw Error('getDriverCapabilities: driver is undefined');

  const result = await runMetadataQuery<{
    capabilities: CapabilitiesResponse['capabilities'];
  }>({
    httpClient,
    body: {
      type: 'get_source_kind_capabilities',
      args: {
        name: driver,
      },
    },
  });

  return result.capabilities;
};
