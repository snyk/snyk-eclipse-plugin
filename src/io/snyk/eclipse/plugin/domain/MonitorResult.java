package io.snyk.eclipse.plugin.domain;

import lombok.Data;

@Data
public class MonitorResult {
	boolean ok;
	String id;
	String uri;
	String path;
	String error;
	
	public static MonitorResult error(String errorMessage) {
		MonitorResult result = new MonitorResult();
		result.error = errorMessage;
		return result;
	}
	
	public boolean hasError() {
		return error != null && !error.isEmpty();
	}

}
