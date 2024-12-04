package io.snyk.languageserver.protocolextension;

import static java.util.Collections.synchronizedSet;

import java.util.HashSet;
import java.util.Set;

import org.eclipse.lsp4j.ProgressParams;
import org.eclipse.lsp4j.WorkDoneProgressCancelParams;
import org.eclipse.lsp4j.WorkDoneProgressEnd;
import org.eclipse.lsp4j.WorkDoneProgressNotification;
import org.eclipse.lsp4j.jsonrpc.messages.Either;

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
		// call language server to cancel
		var workDoneProgressCancelParams = new WorkDoneProgressCancelParams();
		workDoneProgressCancelParams.setToken(token);
		lc.getConnectedLanguageServer().cancelProgress(workDoneProgressCancelParams);

		// call notify progress with end message
		var progressParam = getEndProgressParam(token);
		lc.notifyProgress(progressParam);
		progresses.remove(token);
	}

	public void addProgress(String token) {
		progresses.add(token);
	}

	public void removeProgress(String token) {
		progresses.remove(token);
	}

	public ProgressParams getEndProgressParam(String token) {
		WorkDoneProgressEnd workDoneProgressEnd = new WorkDoneProgressEnd();
		workDoneProgressEnd.setMessage("Operation canceled.");
		Either<WorkDoneProgressNotification, Object> value = Either.forLeft(workDoneProgressEnd);
		Either<String, Integer> tokenEither = Either.forLeft(token);

		var progressParam = new ProgressParams(tokenEither, value);
		return progressParam;
	}

}
