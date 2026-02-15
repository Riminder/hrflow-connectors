/**
 * Clayhr Connector
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


export const ClayhrConnector = new Connector({
  name: 'clayhr',
  type: ConnectorType.ATS,
  workflowType: WorkflowType.PULL,
  actions: [,
});

export default ClayhrConnector;
