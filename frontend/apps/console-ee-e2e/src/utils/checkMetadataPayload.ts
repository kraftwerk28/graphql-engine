/**
 * Freeze and check the request and response payloads.
 *
 * TODO: properly type the interception.
 */

import { Interception } from 'cypress/types/net-stubbing';

type Options = { name?: string };

export function readMetadata() {
  Cypress.log({ message: '**--- Metadata read: start**' });

  return cy
    .request('POST', 'http://localhost:8080/v1/metadata', {
      args: {},
      type: 'export_metadata',
    })
    .then(_response => {
      Cypress.log({ message: '**--- Metadata read: end**' });
    });
}

export function checkMetadataPayload(
  interception: Interception,
  options: Options
) {
  let bodyToSnapshot: unknown;

  // console mode: server
  if (interception.request.url.includes('v1/metadata')) {
    const { resource_version, ...other } = interception.request.body;
    expect(resource_version).to.be.a('number');
    bodyToSnapshot = other.type === 'bulk' ? other.args : other;

    // console mode: cli
  } else {
    bodyToSnapshot = interception.request.body.up;
  }
  cy.wrap({
    bodyToSnapshot,
  }).toMatchSnapshot({ name: options.name });
}
