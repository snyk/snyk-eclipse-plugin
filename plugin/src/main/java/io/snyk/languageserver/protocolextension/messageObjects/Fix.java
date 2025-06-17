package io.snyk.languageserver.protocolextension.messageObjects;

import java.util.Map;

public record Fix (
	String fixId,
	Map<String, String> unifiedDiffsPerFile) {
	// no-arg constructor is generated automatically by Java compiler
}
