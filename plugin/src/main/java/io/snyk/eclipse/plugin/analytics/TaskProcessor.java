package io.snyk.eclipse.plugin.analytics;

import java.util.ArrayList;
import java.util.List;
import java.util.Queue;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.function.Consumer;

import org.apache.commons.lang3.tuple.Pair;

import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.SnykLogger;
import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;

/* 
 * Process Tasks after Language Server is initialized a valid token exists
 */
public class TaskProcessor {
	// left = taskToExecute, right = callback function
	private final Queue<Pair<Consumer<SnykExtendedLanguageClient>, Consumer<Void>>> taskQueue = new ConcurrentLinkedQueue<>();

	private TaskProcessor() {
		CompletableFuture.runAsync(() -> {
			start();
		});
	}

	private static TaskProcessor instance;

	public static TaskProcessor getInstance() {
		synchronized (TaskProcessor.class) {
			if (instance == null) {
				if (instance == null) {
					instance = new TaskProcessor();
				}
			}
		}
		return instance;
	}

	private void start() {
		final List<Pair<Consumer<SnykExtendedLanguageClient>, Consumer<Void>>> copyForSending = new ArrayList<>();

		while (true) {
			String authToken = Preferences.getInstance().getAuthToken();
			SnykExtendedLanguageClient lc = SnykExtendedLanguageClient.getInstance();
			if (taskQueue.isEmpty() || authToken == null || authToken.isBlank() || lc == null) {
				try {
					Thread.sleep(1000);
				} catch (InterruptedException e) {
					Thread.currentThread().interrupt();
				}
				continue;
			}

			copyForSending.clear(); // Clear the list before reuse
			copyForSending.addAll(taskQueue); // Add all elements from taskQueue

			for (Pair<Consumer<SnykExtendedLanguageClient>, Consumer<Void>> event : copyForSending) {
				try {
					// Execute the task with the language client
					event.getLeft().accept(lc);
					// Execute the callback
					if (event.getRight() != null) {
						event.getRight().accept(null);
					}
				} catch (Exception e) {
					SnykLogger.logError(e);
				} finally {
					taskQueue.remove(event);
				}
			}
		}
	}

	public void registerTask(Consumer<SnykExtendedLanguageClient> task, Consumer<Void> callback) {
		var pair = Pair.of(task, callback);
		taskQueue.add(pair);
	}
}