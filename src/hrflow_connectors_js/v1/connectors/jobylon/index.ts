/**
 * Jobylon Connector
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


export const JobylonConnector = new Connector({
  name: 'jobylon',
  type: ConnectorType.ATS,
  workflowType: WorkflowType.PULL,
  actions: [,
});

export default JobylonConnector;
