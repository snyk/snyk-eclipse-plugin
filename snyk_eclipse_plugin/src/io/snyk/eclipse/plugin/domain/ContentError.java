package io.snyk.eclipse.plugin.domain;

import lombok.Data;

@Data
public class ContentError {
	private boolean ok;
	private String error;
	private String path;
}
