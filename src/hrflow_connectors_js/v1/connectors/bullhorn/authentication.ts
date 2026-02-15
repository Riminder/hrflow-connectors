/**
 * Bullhorn Authentication
 * Complete TypeScript translation of authentication.py
 */

import axios from 'axios';

const BASE_URL = 'https://auth.bullhornstaffing.com/oauth';

/**
 * Parse URL query parameters
 */
function parseQueryString(queryString: string): Record<string, string> {
  const params: Record<string, string> = {};
  const pairs = queryString.split('&');
  for (const pair of pairs) {
    const [key, value] = pair.split('=');
    params[decodeURIComponent(key)] = decodeURIComponent(value);
  }
  return params;
}

/**
 * Retrieve the authorization code by initiating the OAuth flow
 */
export async function getAuthCode(
  username: string,
  password: string,
  clientId: string
): Promise<string> {
  const data = {
    client_id: clientId,
    response_type: 'code',
    username,
    password,
    action: 'Login',
  };

  const authorizeUrl = BASE_URL + '/authorize';

  try {
    const response = await axios.post(authorizeUrl, data, {
      maxRedirects: 0,
      validateStatus: (status) => {
        return status >= 200 && status < 400;
      },
    });

    const redirectUrl = response.headers.location || response.request.url;
    const urlParts = redirectUrl.split('?');
    const queryString = urlParts[1] || '';
    const params = parseQueryString(queryString);

    if (!params.code) {
      throw new Error('Authorization code not found in redirect URL');
    }

    return params.code;
  } catch (error) {
    throw new Error(
      `Authorization failed: ${error instanceof Error ? error.message : String(error)}`
    );
  }
}

/**
 * Make a request to obtain the OAuth access token
 */
export async function makeTokenRequest(data: Record<string, any>): Promise<Record<string, any>> {
  const tokenUrl = BASE_URL + '/token';

  try {
    const response = await axios.post(tokenUrl, data);
    if (response.status === 200) {
      return response.data;
    }

    throw new Error(
      `Token request failed with status code ${response.status}: ${response.data}`
    );
  } catch (error) {
    throw new Error(
      `Token request failed: ${error instanceof Error ? error.message : String(error)}`
    );
  }
}

/**
 * Log in to Bullhorn using the obtained access token
 */
export async function loginToBullhorn(
  accessToken: Record<string, any>
): Promise<Record<string, any>> {
  const loginUrl = 'https://rest.bullhornstaffing.com/rest-services/login';
  const params = {
    version: '2.0',
    access_token: accessToken.access_token,
  };

  try {
    const response = await axios.post(loginUrl, {}, { params });

    if (response.status === 200) {
      const authResponse = response.data;
      authResponse.refresh_token = accessToken.refresh_token;
      return authResponse;
    }

    throw new Error(
      `Login to Bullhorn failed with status code ${response.status}: ${response.data}`
    );
  } catch (error) {
    throw new Error(
      `Login failed: ${error instanceof Error ? error.message : String(error)}`
    );
  }
}

/**
 * Gets or refreshes an OAuth access token based on the grant type
 */
export async function getOrRefreshToken(
  grantType: string,
  clientId: string,
  clientSecret: string,
  ttl?: number,
  code?: string,
  refreshToken?: string
): Promise<Record<string, any>> {
  const data: Record<string, any> = {
    grant_type: grantType,
    client_id: clientId,
    client_secret: clientSecret,
  };

  if (grantType === 'authorization_code') {
    data.code = code;
  } else if (grantType === 'refresh_token') {
    data.refresh_token = refreshToken;
  }

  if (ttl) {
    data.ttl = ttl;
  }

  const tokenResponse = await makeTokenRequest(data);
  return await loginToBullhorn(tokenResponse);
}

/**
 * Obtain the access token for authentication
 */
export async function auth(
  username: string,
  password: string,
  clientId: string,
  clientSecret: string,
  refreshToken?: string,
  authCode?: string
): Promise<Record<string, any>> {
  try {
    if (refreshToken) {
      const accessToken = await getOrRefreshToken(
        'refresh_token',
        clientId,
        clientSecret,
        604800,
        undefined,
        refreshToken
      );
      return accessToken;
    } else if (authCode) {
      const accessToken = await getOrRefreshToken(
        'authorization_code',
        clientId,
        clientSecret,
        604800,
        authCode
      );
      return accessToken;
    } else {
      const code = await getAuthCode(username, password, clientId);
      const accessToken = await getOrRefreshToken(
        'authorization_code',
        clientId,
        clientSecret,
        604800,
        code
      );
      return accessToken;
    }
  } catch (error) {
    throw new Error(
      `Authentication failed: ${error instanceof Error ? error.message : String(error)}`
    );
  }
}

export default { auth, getAuthCode, getOrRefreshToken, makeTokenRequest, loginToBullhorn };
