package io.snyk.languageserver.protocolextension;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

class ProgressManagerTest {

	private SnykExtendedLanguageClient lcMock;
	private ProgressManager cut;

	@BeforeEach
	void setUp() {
		this.lcMock = Mockito.mock(SnykExtendedLanguageClient.class);
		this.cut = new ProgressManager(this.lcMock);
	}

	@Test
	void cancelAll_removesAllFromSet() {
		cut.progresses.add("a");
		cut.progresses.add("b");
		
		cut.cancelAll();
		
		assertEquals(0, cut.progresses.size());
	}
	
	@Test
	void cancelAll_callsCancelOnLanguageClient() {
		cut.progresses.add("a");
		cut.progresses.add("b");
		
		cut.cancelAll();
		
		verify(this.lcMock).cancelProgress("a");
		verify(this.lcMock).cancelProgress("b");
	}
	
	@Test
	void add_addsTokenToProgressesSet() {
		cut.progresses.clear();
		
		cut.addProgress("a");
		
		assertEquals(1, cut.progresses.size());
		assertTrue(cut.progresses.contains("a"));
	}
	
	@Test
	void add_addToProgressSett() {
		cut.progresses.add("a");
		
		cut.removeProgress("a");
		
		assertEquals(0, cut.progresses.size());
		verifyNoInteractions(lcMock);
	}

}
