package io.snyk.languageserver.protocolextension.messageObjects.scanResults;

public record AdditionalData(
	// Code
    String message,
    String[] cwe,
    String text,
    boolean isSecurityType,
    boolean hasAIFix,
    // OSS + Code    
    String ruleId,
    String details,
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
    // IaC    
    String publicId,
    String customUIContent
) {
}
