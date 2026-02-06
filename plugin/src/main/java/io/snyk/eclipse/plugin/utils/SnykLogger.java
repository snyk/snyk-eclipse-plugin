package io.snyk.eclipse.plugin.utils;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.statushandlers.StatusManager;

import io.snyk.eclipse.plugin.Activator;

public class SnykLogger {

	public static void logError(Exception exception) {
		Status status = new Status(IStatus.ERROR, Activator.PLUGIN_ID, exception.getMessage(),
				exception);

		StatusManager.getManager().handle(status, StatusManager.SHOW | StatusManager.LOG);
	}

	public static void logInfo(String message) {
		Status status = new Status(IStatus.INFO, Activator.PLUGIN_ID, message);

		StatusManager.getManager().handle(status, StatusManager.LOG);
	}

	public static void logDebug(String message) {
		Status status = new Status(IStatus.OK, Activator.PLUGIN_ID, message);

		StatusManager.getManager().handle(status, StatusManager.LOG);
	}

	public static void logAndShow(String message) {
		Status status = new Status(IStatus.INFO, Activator.PLUGIN_ID, message);

		StatusManager.getManager().handle(status, StatusManager.LOG | StatusManager.SHOW);
	}

}
