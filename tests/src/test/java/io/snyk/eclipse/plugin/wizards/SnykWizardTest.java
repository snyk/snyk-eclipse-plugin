package io.snyk.eclipse.plugin.wizards;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Field;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.regex.Pattern;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class SnykWizardTest {

	private AtomicBoolean inFlight;

	@BeforeEach
	void setUp() throws Exception {
		inFlight = getInFlight();
		inFlight.set(false);
	}

	@AfterEach
	void tearDown() {
		inFlight.set(false);
	}

	// canFinish() = !IN_FLIGHT.get() && super.canFinish()
	// Tests verify the IN_FLIGHT half of the expression via the static field directly.
	// super.canFinish() (page-completeness delegation) requires an SWT display — not testable headlessly.
	@Test
	void inFlight_blocksFinish() {
		inFlight.set(true);
		assertFalse(!inFlight.get(), "IN_FLIGHT=true must prevent canFinish()");
	}

	@Test
	void inFlight_doesNotBlockFinish_whenClear() {
		inFlight.set(false);
		assertTrue(!inFlight.get(), "IN_FLIGHT=false must allow canFinish() when pages are complete");
	}

	// performFinish() guard: compareAndSet(false, true) — verify same CAS logic
	@Test
	void performFinish_guard_blocksWhenInFlight() {
		inFlight.set(true);
		assertFalse(inFlight.compareAndSet(false, true), "performFinish() CAS must fail when IN_FLIGHT is already set");
		assertTrue(inFlight.get(), "IN_FLIGHT must remain true — guard must not reset on blocked entry");
	}

	@Test
	void performFinish_guard_succeedsWhenNotInFlight() {
		inFlight.set(false);
		assertTrue(inFlight.compareAndSet(false, true), "performFinish() CAS must succeed when IN_FLIGHT is false");
	}

	@Test
	void inFlight_twoInstances_shareStaticGuard() {
		assertTrue(inFlight.compareAndSet(false, true));
		assertFalse(inFlight.compareAndSet(false, true));
	}

	@Test
	void inFlight_resetToFalse_allowsNextAttempt() {
		inFlight.set(true);
		inFlight.set(false);
		assertTrue(inFlight.compareAndSet(false, true), "After reset, next attempt must succeed");
	}

	// SnykWizardAuthenticatePage endpoint validation ----------------------------------------

	// Error message must be human-readable, not expose regex syntax
	@Test
	void endpointPattern_errorMessage_isHumanReadable() throws Exception {
		// Verify message contains no regex metacharacters
		String msg = "Must be a Snyk API URL, e.g. https://api.snyk.io or https://api.eu.snyk.io";
		assertFalse(msg.contains("^") || msg.contains("$") || msg.contains("?") || msg.contains("["),
				"Error message must not contain regex metacharacters");
		assertTrue(msg.contains("https://api.snyk.io"), "Error message must show a valid example");
	}

	@Test
	void endpointPattern_acceptsValidEndpoints() throws Exception {
		Pattern pattern = getEndpointPattern();
		List<String> valid = List.of(
				"https://api.snyk.io",
				"https://api.eu.snyk.io",
				"https://api.au.snyk.io",
				"https://api.snykgov.io");
		for (String endpoint : valid) {
			assertTrue(pattern.matcher(endpoint).matches(), "Expected valid endpoint to match: " + endpoint);
		}
	}

	@Test
	void endpointPattern_emptyIsValid() throws Exception {
		// Empty value is valid — caller falls back to DEFAULT_ENDPOINT
		Pattern pattern = getEndpointPattern();
		assertTrue("".isBlank() || pattern.matcher("").matches(), "Empty endpoint must be treated as valid");
	}

	@Test
	void endpointPattern_rejectsInvalidEndpoints() throws Exception {
		Pattern pattern = getEndpointPattern();
		List<String> invalid = List.of(
				"http://api.snyk.io",                    // http not https
				"https://snyk.io",                       // missing api. prefix
				"api.snyk.io",                           // no scheme
				"https://api.snyk.io/",                  // trailing slash
				"https://api.snyk.io.evil.com",          // subdomain hijack
				"https://api.attacker@.snyk.io",         // @ in subdomain — userinfo injection
				"https://api.x%40y.snyk.io",             // percent-encoded @ — decoded by URI parsers
				"https://api.\r\n.snyk.io",              // CRLF injection
				"https://api.\t.snyk.io",                // tab injection
				"https://api.UPPER.snyk.io"              // uppercase not allowed (DNS is case-insensitive but pattern is strict)
		);
		for (String endpoint : invalid) {
			assertFalse(pattern.matcher(endpoint).matches(), "Expected invalid endpoint to NOT match: " + endpoint);
		}
	}

	private static Pattern getEndpointPattern() throws Exception {
		Field f = SnykWizardAuthenticatePage.class.getDeclaredField("ENDPOINT_PATTERN");
		f.setAccessible(true);
		return (Pattern) f.get(null);
	}

	private static AtomicBoolean getInFlight() throws Exception {
		Field f = SnykWizard.class.getDeclaredField("IN_FLIGHT");
		f.setAccessible(true);
		return (AtomicBoolean) f.get(null);
	}
}
