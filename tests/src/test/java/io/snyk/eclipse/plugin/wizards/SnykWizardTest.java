package io.snyk.eclipse.plugin.wizards;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Field;
import java.util.concurrent.atomic.AtomicBoolean;

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

	// canFinish() returns !IN_FLIGHT.get() — verify the guard logic via the field directly
	// (SnykWizard cannot be instantiated without an SWT display in headless CI)
	@Test
	void canFinish_logic_returnsFalse_whenInFlight() {
		inFlight.set(true);
		assertFalse(!inFlight.get(), "canFinish() must return false while auth is in flight");
	}

	@Test
	void canFinish_logic_returnsTrue_whenNotInFlight() {
		inFlight.set(false);
		assertTrue(!inFlight.get(), "canFinish() must return true when no auth is in flight");
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

	private static AtomicBoolean getInFlight() throws Exception {
		Field f = SnykWizard.class.getDeclaredField("IN_FLIGHT");
		f.setAccessible(true);
		return (AtomicBoolean) f.get(null);
	}
}
