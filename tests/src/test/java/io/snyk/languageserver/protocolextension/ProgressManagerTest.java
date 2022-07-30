package io.snyk.languageserver.protocolextension;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.lsp4j.ProgressParams;
import org.eclipse.lsp4j.WorkDoneProgressBegin;
import org.eclipse.lsp4j.WorkDoneProgressCreateParams;
import org.eclipse.lsp4j.WorkDoneProgressEnd;
import org.eclipse.lsp4j.WorkDoneProgressReport;
import org.eclipse.lsp4j.jsonrpc.messages.Either;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

class ProgressManagerTest {

  @BeforeEach
  void setUp() {
  }

  @Test
  void testCreateProgress_shouldCreateJobAndAddItToMapWithToken() {
    ProgressManager.SnykBackgroundJobBuilder jobBuilderMock = mock(ProgressManager.SnykBackgroundJobBuilder.class);
    ProgressManager progressManager = new ProgressManager(jobBuilderMock);
    WorkDoneProgressCreateParams param = new WorkDoneProgressCreateParams();
    param.setToken("testToken");
    ProgressManager.SnykBackgroundJob jobMock = mock(ProgressManager.SnykBackgroundJob.class);
    when(jobBuilderMock.build(param)).thenReturn(jobMock);

    progressManager.createProgress(param);

    assertEquals(1, progressManager.progresses.size());
    assertNotNull(progressManager.progresses.get(progressManager.getToken(param.getToken())));
    verify(jobMock, times(1)).schedule();
  }

  @SuppressWarnings("BusyWait")
  @Test
  void doWork_shouldWaitUntilJobRemovedFromProgressesMap() {
    WorkDoneProgressCreateParams param = new WorkDoneProgressCreateParams();
    param.setToken("testToken");
    ProgressManager pm = new ProgressManager();
    String key = pm.getToken(param.getToken());
    ProgressManager.SnykBackgroundJob job = pm.jobBuilder.build(param);
    pm.progresses.put(key, new ImmutablePair<>(job, null));
    IProgressMonitor monitor = mock(IProgressMonitor.class);
    // wait for the progress monitor to be added... and make sure it waits
    Thread t = new Thread(() -> {
      while (pm.progresses.get(key) == null || pm.progresses.get(key).getValue() == null) {
        try {
          Thread.sleep(10);
        } catch (InterruptedException e) {
          Thread.currentThread().interrupt();
        } finally {
          pm.progresses.remove(key);
        }
      }
    });
    t.start();
    try {
      Thread.sleep(30);
    } catch (InterruptedException e) {
      Thread.currentThread().interrupt();
    }

    // interacts with the previous thread - test that it puts progress monitor into map
    // and finishes once the token is cleared from the map
    pm.doWork(job, monitor, param);
    assertEquals(0, pm.currentPercentage.get(monitor));
  }

  @Test
  void updateProgress_ShouldUpdatePercentage() {
    ProgressManager pm = new ProgressManager();
    IProgressMonitor monitor = mock(IProgressMonitor.class);
    ProgressParams progressParams = setupUpdateTest(pm, monitor);

    pm.updateProgress(progressParams);

    assertEquals(23, pm.currentPercentage.get(monitor));
  }

  @Test
  void updateProgress_EndShouldCallDoneAndCleanMaps() {
    ProgressManager pm = new ProgressManager();
    IProgressMonitor monitor = mock(IProgressMonitor.class);
    ProgressParams progressParams = setupUpdateTest(pm, monitor);
    progressParams.setValue(Either.forLeft(new WorkDoneProgressEnd()));

    pm.updateProgress(progressParams);

    verify(monitor).done();
    assertNull(pm.currentPercentage.get(monitor));
    assertNull(pm.progresses.get(pm.getToken(progressParams.getToken())));
  }

  @Test
  void updateProgress_BeginShouldSetTitleAndMaxAmountAndStartSubtaskWithMessage() {
    ProgressManager pm = new ProgressManager();
    IProgressMonitor monitor = mock(IProgressMonitor.class);
    ProgressParams progressParams = setupUpdateTest(pm, monitor);
    WorkDoneProgressBegin begin = new WorkDoneProgressBegin();
    String title = "title";
    String message = "message";
    begin.setTitle(title);
    begin.setMessage(message);
    begin.setPercentage(1);
    progressParams.setValue(Either.forLeft(begin));

    pm.updateProgress(progressParams);

    verify(monitor).beginTask(title, 100);
    verify(monitor).subTask(message);
  }

  @Test
  void updateProgress_BeginShouldAcceptEmptyTitle() {
    ProgressManager pm = new ProgressManager();
    IProgressMonitor monitor = mock(IProgressMonitor.class);
    ProgressParams progressParams = setupUpdateTest(pm, monitor);
    WorkDoneProgressBegin begin = new WorkDoneProgressBegin();
    String title = "";
    begin.setTitle(title);
    String message = "msg";
    begin.setMessage(message);
    begin.setPercentage(1);
    progressParams.setValue(Either.forLeft(begin));

    pm.updateProgress(progressParams);

    verify(monitor).beginTask(title, 100);
    verify(monitor).subTask(message);
  }

  @Test
  void updateProgress_BeginShouldAcceptEmptyMessage() {
    ProgressManager pm = new ProgressManager();
    IProgressMonitor monitor = mock(IProgressMonitor.class);
    ProgressParams progressParams = setupUpdateTest(pm, monitor);
    WorkDoneProgressBegin begin = new WorkDoneProgressBegin();
    String title = "title";
    begin.setTitle(title);
    String message = "";
    begin.setMessage(message);
    begin.setPercentage(1);
    progressParams.setValue(Either.forLeft(begin));

    pm.updateProgress(progressParams);

    verify(monitor).beginTask(title, 100);
    verifyNoMoreInteractions(monitor);
  }

  @Test
  void updateProgress_BeginShouldAcceptNullMessage() {
    ProgressManager pm = new ProgressManager();
    IProgressMonitor monitor = mock(IProgressMonitor.class);
    ProgressParams progressParams = setupUpdateTest(pm, monitor);
    WorkDoneProgressBegin begin = new WorkDoneProgressBegin();
    begin.setTitle("title");
    begin.setMessage(null);
    progressParams.setValue(Either.forLeft(begin));

    pm.updateProgress(progressParams);

    verify(monitor).beginTask("title", -1);
    verifyNoMoreInteractions(monitor);
  }

  private ProgressParams setupUpdateTest(ProgressManager pm, IProgressMonitor monitor) {
    ProgressParams progressParams = new ProgressParams();
    progressParams.setToken("testToken");
    WorkDoneProgressReport report = new WorkDoneProgressReport();
    report.setPercentage(23);
    progressParams.setValue(Either.forLeft(report));
    ProgressManager.SnykBackgroundJob job = mock(ProgressManager.SnykBackgroundJob.class);
    var pair = new ImmutablePair<>(job, monitor);
    pm.progresses.put(pm.getToken(progressParams.getToken()), pair);
    return progressParams;
  }
}
