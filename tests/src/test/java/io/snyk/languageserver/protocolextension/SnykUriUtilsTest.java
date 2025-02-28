package io.snyk.languageserver.protocolextension;

import static org.junit.jupiter.api.Assertions.*;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.Test;

import io.snyk.eclipse.plugin.utils.UriUtils;

class SnykUriUtilsTest {

	@Test
	public void getDecodedParam_Returns_Parameter_Value_If_Present_In_Query_String()
			throws URISyntaxException {
		String query = "product=Snyk+Code&issueId=7642f506c568056a7090d3ceb7b3c2e0&action=showInDetailPanel";
		URI uriWithQuery = new URI("snyk://path/to/resource?" + query);

		var result = UriUtils.getDecodedParam(uriWithQuery, "issueId");
		assertEquals("7642f506c568056a7090d3ceb7b3c2e0", result);

		result = UriUtils.getDecodedParam(uriWithQuery, "action");
		assertEquals("showInDetailPanel", result);

		result = UriUtils.getDecodedParam(uriWithQuery, "product");
		assertEquals("Snyk Code", result);
	}

	@Test
	void getDecodedParamNullQuery() throws URISyntaxException {
		URI uri = new URI(
				"snyk:///test_repos/project-with-vulns/routes/vulnCodeSnippet.ts");
		String paramName = "product";

		assertNull(UriUtils.getDecodedParam(uri, paramName));
	}

	@Test
	void getDecodedParamEmptyQuery() throws URISyntaxException {
		URI uri = new URI(
				"snyk:///test_repos/project-with-vulns/routes/vulnCodeSnippet.ts?product=");
		String paramName = "product";

		assertNull(UriUtils.getDecodedParam(uri, paramName));
	}

	@Test
	void getDecodedParamMissingParameter() throws URISyntaxException {
		URI uri = new URI(
				"snyk:///test_repos/project-with-vulns/routes/vulnCodeSnippet.ts?other=parameter");
		String paramName = "product";

		assertNull(UriUtils.getDecodedParam(uri, paramName));
	}

	@Test
	public void parseQueryString_Returns_Parameters_In_Query_String()
			throws Exception {
		String query = "product=Snyk+Code&issueId=7642f506c568056a7090d3ceb7b3c2e0&action=showInDetailPanel";
		URI uriWithQuery = new URI("snyk://path/to/resource?" + query);

		var result = UriUtils.getQueryParameters(uriWithQuery.getQuery());
		assertEquals(3, result.size());
	}

	@Test
	public void parseQueryString_Returns_Empty_Map_If_Query_String_Is_Empty()
			throws URISyntaxException {
		var result = UriUtils.getQueryParameters(null);
		assertTrue(result.isEmpty());
	}

	@Test
	void queryParametersEmptyQueryString() {
		Map<String, String> paramMap = UriUtils.getQueryParameters("");

		assertTrue(paramMap.isEmpty());
	}

	@Test
	void queryParametersNullQueryString() {
		Map<String, String> paramMap = UriUtils.getQueryParameters(null);

		assertTrue(paramMap.isEmpty());
	}

	@Test
	void queryParametersSingleParameter() {
		String queryString = "product=Snyk+Code";
		Map<String, String> expectedParamMap = Collections
				.singletonMap("product", "Snyk Code");

		Map<String, String> paramMap = UriUtils.getQueryParameters(queryString);

		assertEquals(expectedParamMap, paramMap);
	}

	@Test
	void queryParametersMultipleParameters() {
		String queryString = "product=Snyk+Code&other=parameter";
	    Map<String, String> expectedParamMap = new HashMap<>();
	    expectedParamMap.put("product", "Snyk Code");
	    expectedParamMap.put("other", "parameter");


		Map<String, String> paramMap = UriUtils.getQueryParameters(queryString);

		assertEquals(expectedParamMap, paramMap);
	}

}
