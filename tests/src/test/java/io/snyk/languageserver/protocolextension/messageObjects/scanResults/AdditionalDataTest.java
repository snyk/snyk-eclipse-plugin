package io.snyk.languageserver.protocolextension.messageObjects.scanResults;

import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;

import org.instancio.Instancio;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import io.snyk.languageserver.LsBaseTest;
import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;

class AdditionalDataTest extends LsBaseTest {
	private SnykExtendedLanguageClient lcMock;

	@BeforeEach
	protected void setUp() {
		super.setUp();
		lcMock = Mockito.mock(SnykExtendedLanguageClient.class);
	}

	@AfterEach
	protected void tearDown() {
		super.tearDown();
	}

	@Test
	void testGetDetails() {
		try (MockedStatic<SnykExtendedLanguageClient> staticLanguageClientMock = mockStatic(SnykExtendedLanguageClient.class)) {
			staticLanguageClientMock.when(() -> SnykExtendedLanguageClient.getInstance()).thenReturn(lcMock);

			AdditionalData testData = getTestDataRecord();
			testData.customUIContent();

			verify(lcMock).getIssueDescription(testData.key());
		}
	}

	private AdditionalData getTestDataRecord() {
		return Instancio.create(AdditionalData.class);
	}

}
