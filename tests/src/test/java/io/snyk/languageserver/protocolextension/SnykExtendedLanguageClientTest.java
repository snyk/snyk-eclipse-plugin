package io.snyk.languageserver.protocolextension;

import org.eclipse.lsp4j.ProgressParams;
import org.eclipse.lsp4j.WorkDoneProgressCreateParams;
import org.junit.jupiter.api.Test;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

class SnykExtendedLanguageClientTest {
  @Test
  void createProgress() {
    var pm = mock(ProgressManager.class);
    SnykExtendedLanguageClient lc = new SnykExtendedLanguageClient(pm);
    WorkDoneProgressCreateParams params = new WorkDoneProgressCreateParams();
    lc.createProgress(params);
    verify(pm).createProgress(params);
  }

  @Test
  void notifyProgress() {
    var pm = mock(ProgressManager.class);
    SnykExtendedLanguageClient lc = new SnykExtendedLanguageClient(pm);
    ProgressParams params = new ProgressParams();
    lc.notifyProgress(params);
    verify(pm).updateProgress(params);
  }
}
