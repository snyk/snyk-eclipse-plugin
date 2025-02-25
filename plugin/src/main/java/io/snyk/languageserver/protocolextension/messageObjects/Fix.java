package io.snyk.languageserver.protocolextension.messageObjects;

import java.util.Map;

import com.google.gson.annotations.SerializedName;

public record Fix(
		@SerializedName("fixId") String fixId,
		@SerializedName("unifiedDiffsPerFile") Map<String, String> unifiedDiffsPerFile) {
	// no-arg constructor is generated automatically by Java compiler
}