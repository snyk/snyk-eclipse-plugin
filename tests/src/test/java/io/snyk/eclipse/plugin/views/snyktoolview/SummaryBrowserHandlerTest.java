package io.snyk.eclipse.plugin.views.snyktoolview;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import io.snyk.eclipse.plugin.preferences.InMemoryPreferenceStore;
import io.snyk.eclipse.plugin.preferences.InMemorySecurePreferenceStore;
import io.snyk.eclipse.plugin.preferences.Preferences;

class SummaryBrowserHandlerTest {

	// Counts renderSummary invocations so the dedup guard is observable without a live SWT Browser.
	// The browser is null; renderSummary's BrowserFlashGuard call is a no-op on a null browser, but the
	// hasSummary/lastSummary bookkeeping (which the guard reads) still runs via super.
	private static final class CountingSummaryBrowserHandler extends SummaryBrowserHandler {
		int renders;

		CountingSummaryBrowserHandler() {
			super(null);
		}

		@Override
		void renderSummary(String summary) {
			renders++;
			super.renderSummary(summary);
		}
	}

	@BeforeEach
	void setUp() {
		Preferences prefs = Preferences.getTestInstance(new InMemoryPreferenceStore(), new InMemorySecurePreferenceStore());
		prefs.setTest(true);
	}

	@Test
	void setBrowserText_rendersOnFirstCall() {
		CountingSummaryBrowserHandler handler = new CountingSummaryBrowserHandler();

		handler.setBrowserText("summary-a");

		assertEquals(1, handler.renders, "first summary must render");
	}

	@Test
	void setBrowserText_skipsIdenticalConsecutiveSummary() {
		CountingSummaryBrowserHandler handler = new CountingSummaryBrowserHandler();

		handler.setBrowserText("summary-a");
		handler.setBrowserText("summary-a");

		assertEquals(1, handler.renders, "an identical repeated summary must be deduped (not re-rendered)");
	}

	@Test
	void setBrowserText_rendersWhenSummaryChanges() {
		CountingSummaryBrowserHandler handler = new CountingSummaryBrowserHandler();

		handler.setBrowserText("summary-a");
		handler.setBrowserText("summary-b");

		assertEquals(2, handler.renders, "a changed summary must render");
	}

	@Test
	void setBrowserText_rendersAgainAfterDefaultReset() {
		CountingSummaryBrowserHandler handler = new CountingSummaryBrowserHandler();

		handler.setBrowserText("summary-a");
		handler.setDefaultBrowserText(); // resets hasSummary
		handler.setBrowserText("summary-a");

		assertEquals(2, handler.renders, "after the default page reset, the same summary text must render again");
	}
}
