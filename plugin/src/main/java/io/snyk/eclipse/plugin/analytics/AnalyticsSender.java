package io.snyk.eclipse.plugin.analytics;

import java.util.LinkedList;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.function.Consumer;

import org.apache.commons.lang3.tuple.Pair;

import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.SnykLogger;
import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;

public class AnalyticsSender {
	// left = event, right = callback function
	private final ConcurrentLinkedQueue<Pair<AbstractAnalyticsEvent, Consumer<Void>>> eventQueue = new ConcurrentLinkedQueue<>();

	private AnalyticsSender() {
		CompletableFuture.runAsync(() -> { start(); });
	}

	private static AnalyticsSender instance;

	public static AnalyticsSender getInstance() {
		if (instance == null) {
			synchronized (AnalyticsSender.class) {
				if (instance == null) {
					instance = new AnalyticsSender();
				}
			}
		}
		return instance;
	}

	private void start() {
		while (true) {
			String authToken = Preferences.getInstance().getAuthToken();
			var lc = SnykExtendedLanguageClient.getInstance();
			if (eventQueue.isEmpty() || authToken == null || authToken.isBlank() || lc == null ) {
				try {
					Thread.sleep(1000);
				} catch (InterruptedException e) {
					Thread.currentThread().interrupt();
				}
				continue;
			}
			LinkedList<Pair<AbstractAnalyticsEvent, Consumer<Void>>> copyForSending = new LinkedList<>(eventQueue);
			for (Pair<AbstractAnalyticsEvent, Consumer<Void>> event : copyForSending) {
				try {
					lc.reportAnalytics(event.getLeft());
					event.getRight().accept(null);
				} catch (Exception e) {
					SnykLogger.logError(e);
				} finally {
					eventQueue.remove(event);
				}
			}
		}
	}

	public void logEvent(AbstractAnalyticsEvent event, Consumer<Void> callback) {
		var pair = Pair.of(event, callback);
		eventQueue.add(pair);
	}
}