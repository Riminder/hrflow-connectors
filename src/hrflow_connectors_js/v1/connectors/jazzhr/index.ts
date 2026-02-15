/**
 * Jazzhr Connector
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


export const JazzhrConnector = new Connector({
  name: 'jazzhr',
  type: ConnectorType.ATS,
  workflowType: WorkflowType.PULL,
  actions: [,
});

export default JazzhrConnector;
