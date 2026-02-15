/**
 * Harbourats Connector
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


export const HarbouratsConnector = new Connector({
  name: 'harbourats',
  type: ConnectorType.ATS,
  workflowType: WorkflowType.PULL,
  actions: [,
});

export default HarbouratsConnector;
