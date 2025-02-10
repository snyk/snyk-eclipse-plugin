package io.snyk.languageserver.protocolextension.messageObjects;

public record ScanCommandConfig(
		String preScanCommand, 
		boolean preScanOnlyReferenceFolder,
		String postScanCommand,
		boolean postScanOnlyReferenceFolder) {
}
