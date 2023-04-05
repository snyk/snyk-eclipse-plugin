package io.snyk.languageserver.protocolextension.messageObjects;

public class OAuthToken {
  // AccessToken is the token that authorizes and authenticates
  // the requests.
  private String accessToken;

  // TokenType is the type of token.
  // The Type method returns either this or "Bearer", the default.
  private String tokenType;

  // RefreshToken is a token that's used by the application
  // (as opposed to the user) to refresh the access token
  // if it expires.
  private String refreshToken;

  // Expiry is the optional expiration time of the access token.
  //
  // If null, TokenSource implementations will reuse the same
  // token forever and RefreshToken or equivalent
  // mechanisms for that TokenSource will not be used.
  private String expiry;

  // raw optionally contains extra metadata from the server
  // when updating a token.
  private Object raw;

  public void Token() {
      // default constructor
  }

  public String getAccessToken() {
      return accessToken;
  }

  public void setAccessToken(String accessToken) {
      this.accessToken = accessToken;
  }

  public String getTokenType() {
      return tokenType;
  }

  public void setTokenType(String tokenType) {
      this.tokenType = tokenType;
  }

  public String getRefreshToken() {
      return refreshToken;
  }

  public void setRefreshToken(String refreshToken) {
      this.refreshToken = refreshToken;
  }

  public String getExpiry() {
      return expiry;
  }

  public void setExpiry(String expiry) {
      this.expiry = expiry;
  }

  public Object getRaw() {
      return raw;
  }

  public void setRaw(Object raw) {
      this.raw = raw;
  }
}
