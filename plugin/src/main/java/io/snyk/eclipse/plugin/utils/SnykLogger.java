package io.snyk.eclipse.plugin.utils;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.statushandlers.StatusManager;

public class SnykLogger {

  public static void logError(Exception exception) {
    Status status = new Status(IStatus.ERROR, PlatformUI.PLUGIN_ID, "Snyk plugin: " + exception.getMessage(), exception);

    StatusManager.getManager().handle(status, StatusManager.SHOW | StatusManager.LOG);
  }
}
