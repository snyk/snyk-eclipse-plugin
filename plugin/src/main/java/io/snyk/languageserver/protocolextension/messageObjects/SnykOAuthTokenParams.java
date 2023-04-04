package io.snyk.languageserver.protocolextension.messageObjects;

public class SnykOAuthTokenParams {
  // will look something like '{"access_token":"configAccessToken","token_type":"Bearer", refresh_token":"configRefreshToken","expiry":"3023-03-29T17:47:13.714448+02:00"}'
  private String tokenPayload;

  public String getToken() {
    // TODO: parse the token payload and get the token
    return tokenPayload;
  }

  public void setToken(String tokenPayload) {
    this.tokenPayload = tokenPayload;
  }
}
