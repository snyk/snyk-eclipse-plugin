package io.snyk.languageserver.protocolextension.messageObjects.scanResults;

import java.util.List;

import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;

public record AdditionalData(
	String key,
	// Code
    String message,
    String[] cwe,
    String text,
    boolean hasAIFix,
    // OSS + Code
    String ruleId,
    String license,
    String description,
    String language,
    String packageManager,
    String packageName,
    String name,
    String version,
    String exploit,
    String projectName,
    String displayTargetFile,
    boolean isUpgradable,
    // IaC
    String publicId,
    List<String> path
) {
	public String customUIContent() {
		var result = SnykExtendedLanguageClient.getInstance().getIssueDescription(key);
		return result;
	}
}
