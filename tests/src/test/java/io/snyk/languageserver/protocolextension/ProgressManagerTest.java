package io.snyk.languageserver.protocolextension;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import org.eclipse.lsp4j.services.LanguageServer;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

class ProgressManagerTest {

	private SnykExtendedLanguageClient lcMock;
	private ProgressManager cut;
	private LanguageServer lsMock;

	@BeforeEach
	void setUp() {
		this.lcMock = mock(SnykExtendedLanguageClient.class);
		this.lsMock = mock(LanguageServer.class);
		this.cut = new ProgressManager(this.lcMock);
		when(lcMock.getConnectedLanguageServer()).thenReturn(lsMock);
		
	}

	@Test
	void cancelAll_removesAllFromSet() {
		cut.progresses.add("a");
		cut.progresses.add("b");
		
		cut.cancelAll();
		
		assertEquals(0, cut.progresses.size());
	}
	
	@Test
	void cancelAll_callsCancelOnLanguageServer() {
		cut.progresses.add("a");
		cut.progresses.add("b");
		
		cut.cancelAll();
		

		verify(lsMock, times(2)).cancelProgress(Mockito.any());
	}

	
	@Test
	void add_addsTokenToProgressesSet() {
		cut.progresses.clear();
		
		cut.addProgress("a");
		
		assertEquals(1, cut.progresses.size());
		assertTrue(cut.progresses.contains("a"));
	}
	
	@Test
	void add_addToProgressSet() {
		cut.progresses.add("a");
		
		cut.removeProgress("a");
		
		assertEquals(0, cut.progresses.size());
		verifyNoInteractions(lcMock);
	}

}
