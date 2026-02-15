/**
 * Occupop Connector
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


export const OccupopConnector = new Connector({
  name: 'occupop',
  type: ConnectorType.ATS,
  workflowType: WorkflowType.PULL,
  actions: [,
});

export default OccupopConnector;
