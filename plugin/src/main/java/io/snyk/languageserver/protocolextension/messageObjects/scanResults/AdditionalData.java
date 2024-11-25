package io.snyk.languageserver.protocolextension.messageObjects.scanResults;

import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;

public record AdditionalData(
	String key,
	// Code
    String message,
    String[] cwe,
    String text,
    boolean isSecurityType,
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
    String publicId
) {
	public String customUIContent() {
		return SnykExtendedLanguageClient.getInstance().getIssueDescription(key);
	}
}
