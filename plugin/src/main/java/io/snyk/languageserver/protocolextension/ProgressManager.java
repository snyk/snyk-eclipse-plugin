package io.snyk.languageserver.protocolextension;

import static java.util.Collections.synchronizedSet;

import java.util.HashSet;
import java.util.Set;

public class ProgressManager {
	SnykExtendedLanguageClient lc = SnykExtendedLanguageClient.getInstance();
	Set<String> progresses = synchronizedSet(new HashSet<>());

	ProgressManager(SnykExtendedLanguageClient lc) {
		this.lc = lc;
	}

	public void cancelAll() {
		HashSet<String> copy = new HashSet<>(progresses);
		for (String token : copy) {
			cancelProgress(token);
		}
	}

	public void cancelProgress(String token) {
		lc.cancelProgress(token);
		progresses.remove(token);
	}

	public void addProgress(String token) {
		progresses.add(token);
	}

	public void removeProgress(String token) {
		progresses.remove(token);
	}

}
