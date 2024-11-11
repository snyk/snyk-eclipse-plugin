package io.snyk.languageserver.protocolextension.messageObjects.scanResults;

public record IgnoreDetails(String category, String reason, String expiration, 
		String ignoredOn, String ignoredBy){}
