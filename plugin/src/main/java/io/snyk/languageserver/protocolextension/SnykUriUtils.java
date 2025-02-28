package io.snyk.languageserver.protocolextension;

import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.net.URLDecoder;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import io.snyk.eclipse.plugin.utils.SnykLogger;

public class SnykUriUtils {
	
	public static String getDecodedParam(URI uri, String paramName) {
		String query = uri.getQuery();
		if (query == null || query.isEmpty()) {
			return null;
		}

		Map<String, String> paramMap = getQueryParameters(query);
		String value = paramMap.get(paramName);

		if (value == null) {
			return null;
		}
		
		return value;
	}

	static Map<String, String> getQueryParameters(String queryString) {
		Map<String, String> paramMap = new HashMap<>();

		if (queryString == null || queryString.isEmpty()) {
			return paramMap;
		}

		for (String param : queryString.split("&")) {
			if (!param.isEmpty()) {
				String[] keyValue = param.split("=");

				if (keyValue.length == 2) { // NOPMD, AvoidLiteralsInIfCondition
					try {
						paramMap.put(keyValue[0],
								URLDecoder.decode(keyValue[1], "UTF-8"));
					} catch (UnsupportedEncodingException e) {
						SnykLogger.logError(e);
						return Collections.emptyMap();
					}
				}
			}
		}

		return paramMap;
	}
}