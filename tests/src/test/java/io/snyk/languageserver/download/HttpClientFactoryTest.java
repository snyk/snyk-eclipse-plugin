package io.snyk.languageserver.download;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import org.eclipse.core.net.proxy.IProxyData;
import org.junit.jupiter.api.Test;

import io.snyk.languageserver.LsBaseTest;

class HttpClientFactoryTest extends LsBaseTest {

  @Test
  void factoryShouldUseGivenHttpsProxyData() {
    IProxyData data = getDummyProxyData();
    when(proxyServiceMock.getProxyData()).thenReturn(new IProxyData[] { data });
    when(proxyServiceMock.select(any())).thenReturn(new IProxyData[] { data });
    HttpClientFactory cut = HttpClientFactory.getInstance();

    cut.create(environment);

    assertNotNull(cut.getContext().getAuthCache());
  }

  private IProxyData getDummyProxyData() {
    IProxyData data = new IProxyData() {
      @Override
      public String getType() {
        return IProxyData.HTTPS_PROXY_TYPE;
      }

      @Override
      public String getHost() {
        return "proxy";
      }

      @Override
      public void setHost(String host) {
      }

      @Override
      public int getPort() {
        return 3128;
      }

      @Override
      public void setPort(int port) {

      }

      @Override
      public String getUserId() {
        return "testUserId";
      }

      @Override
      public void setUserid(String userid) {

      }

      @Override
      public String getPassword() {
        return "testPass";
      }

      @Override
      public void setPassword(String password) {

      }

      @Override
      public boolean isRequiresAuthentication() {
        return true;
      }

      @Override
      public void disable() {

      }
    };
    return data;
  }

}
