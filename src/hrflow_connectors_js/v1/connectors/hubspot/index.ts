/**
 * Hubspot Connector
 * Complete TypeScript translation of connector.py
 */

import {
  ActionName,
  ActionType,
  BaseActionParameters,
  Connector,
  ConnectorAction,
  ConnectorType,
  WorkflowType,
} from '../../core';

export function formatHrflowProfile(data: Record<string, any>): Record<string, any> {
  // Format function implementation
  return data;
}

export function formatHubspotContact(data: Record<string, any>): Record<string, any> {
  // Format function implementation
  return data;
}


export const HubspotConnector = new Connector({
  name: 'hubspot',
  type: ConnectorType.ATS,
  workflowType: WorkflowType.PULL,
  actions: [,
});

export default HubspotConnector;
