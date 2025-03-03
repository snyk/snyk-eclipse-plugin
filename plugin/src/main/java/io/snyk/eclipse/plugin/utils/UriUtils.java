package io.snyk.eclipse.plugin.utils;

import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.net.URLDecoder;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

public class UriUtils {

	/**
	 * Retrieves a parameter value from the given URI.
	 *
	 * If the query string does not contain the specified parameter or is
	 * null/empty, this method returns null.
	 *
	 * @param uri
	 *            The URI to retrieve parameters from
	 * @param paramName
	 *            The name of the parameter to retrieve
	 * @return The parameter value if it exists in the query string, otherwise
	 *         null
	 */
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

	/**
	 * Parses a query string into a map of key-value pairs, decoding any
	 * URL-encoded values.
	 *
	 * If the input is null or empty, an empty map is returned. If an encoding
	 * error occurs while decoding a value, an error is logged and an empty map
	 * is returned.
	 *
	 * @param queryString
	 *            The query string to parse
	 * @return A map containing the decoded parameters
	 */
	public static Map<String, String> getQueryParameters(String queryString) {
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