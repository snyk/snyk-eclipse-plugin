package io.snyk.languageserver.protocolextension;

import io.snyk.eclipse.plugin.utils.SnykLogger;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.lsp4j.ProgressParams;
import org.eclipse.lsp4j.WorkDoneProgressBegin;
import org.eclipse.lsp4j.WorkDoneProgressCreateParams;
import org.eclipse.lsp4j.WorkDoneProgressEnd;
import org.eclipse.lsp4j.WorkDoneProgressNotification;
import org.eclipse.lsp4j.WorkDoneProgressReport;
import org.eclipse.lsp4j.jsonrpc.messages.Either;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;

@SuppressWarnings("BusyWait")
public class ProgressManager {
  final ConcurrentHashMap<String, Pair<SnykBackgroundJob, IProgressMonitor>> progresses = new ConcurrentHashMap<>();
  final ConcurrentHashMap<IProgressMonitor, Integer> currentPercentage = new ConcurrentHashMap<>();
  SnykBackgroundJobBuilder jobBuilder = new SnykBackgroundJobBuilder();

  ProgressManager(SnykBackgroundJobBuilder jobBuilder) {
    this.jobBuilder = jobBuilder;
  }

  ProgressManager() {
  }

  class SnykBackgroundJobBuilder {
    SnykBackgroundJob build(WorkDoneProgressCreateParams param) {
      return new SnykBackgroundJob(param);
    }
  }

  class SnykBackgroundJob extends Job {
    private final WorkDoneProgressCreateParams param;

    public SnykBackgroundJob(WorkDoneProgressCreateParams param) {
      super("Snyk Background Job");
      this.param = param;
    }

    protected IStatus run(IProgressMonitor monitor) {
      IStatus cancelStatus = doWork(progresses.get(getToken(param.getToken())).getKey(), monitor, param);
      if (cancelStatus != null)
        return cancelStatus;
      return Status.OK_STATUS;
    }
  }

  public CompletableFuture<Void> createProgress(WorkDoneProgressCreateParams param) {
    synchronized (progresses) {
      var job = jobBuilder.build(param);
      progresses.put(getToken(param.getToken()), new ImmutablePair<>(job, null));
      job.schedule();
      return CompletableFuture.completedFuture(null);
    }
  }

  IStatus doWork(SnykBackgroundJob job, IProgressMonitor monitor, WorkDoneProgressCreateParams param) {
    var token = getToken(param.getToken());
    progresses.put(token, new ImmutablePair<>(job, monitor));
    currentPercentage.put(monitor, 0);
    while (progresses.containsKey(token)) {
      try {
        if (monitor.isCanceled()) {
          return Status.CANCEL_STATUS;
        }
        Thread.sleep(100);
      } catch (InterruptedException e) {
        SnykLogger.logError(e);
        Thread.currentThread().interrupt();
      }
    }
    return null;
  }

  String getToken(Either<String, Integer> param) {
    return param.getLeft();
  }

  public void updateProgress(ProgressParams param) {
    String progressToken = getToken(param.getToken());
    while (!progresses.containsKey(progressToken) || progresses.get(progressToken).getValue() == null) {
      try {
        Thread.sleep(1000);
      } catch (InterruptedException e) {
        SnykLogger.logError(e);
        Thread.currentThread().interrupt();
      }
    }
    synchronized (progresses) {
      WorkDoneProgressNotification notification = param.getValue().getLeft();
      IProgressMonitor monitor = progresses.get(progressToken).getValue();
      switch (notification.getKind()) {
        case begin:
          begin((WorkDoneProgressBegin) notification, monitor);
          break;
        case report:
          report((WorkDoneProgressReport) notification, monitor);
          break;
        case end:
          end((WorkDoneProgressEnd) notification, monitor);
          progresses.remove(progressToken);
          currentPercentage.remove(monitor);
          break;
      }
    }
  }

  private void end(WorkDoneProgressEnd end, IProgressMonitor monitor) {
    if (null == end || null == monitor)
      return;
    monitor.subTask(end.getMessage());
    monitor.done();
  }

  private void report(WorkDoneProgressReport report, IProgressMonitor monitor) {
    if (report.getPercentage() == null) return;
    var worked = currentPercentage.get(monitor) != null
      ? Math.min(currentPercentage.get(monitor), report.getPercentage())
      : 0;

    if (null != report.getMessage() && !report.getMessage().isBlank()) {
      monitor.subTask(report.getMessage());
    }

    monitor.worked(report.getPercentage() - worked);
    currentPercentage.put(monitor, report.getPercentage());
  }

  private void begin(WorkDoneProgressBegin begin, IProgressMonitor monitor) {
    if (null == begin || null == monitor)
      return;
    monitor.beginTask(begin.getTitle(), 100);
    String message = begin.getMessage();
    if (message != null && !message.isBlank())
      monitor.subTask(message);
  }

}
