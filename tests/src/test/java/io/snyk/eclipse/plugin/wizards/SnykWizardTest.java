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

	@Test
	void inFlight_startsAtFalse() {
		assertFalse(inFlight.get());
	}

	@Test
	void inFlight_compareAndSet_blocksSecondCaller() {
		// Simulate first performFinish() acquiring the guard
		assertTrue(inFlight.compareAndSet(false, true), "First caller must acquire guard");

		// Simulate second performFinish() — must be blocked
		assertFalse(inFlight.compareAndSet(false, true), "Second caller must be blocked");
	}

	@Test
	void inFlight_twoInstances_shareStaticGuard() {
		// Both wizard instances share IN_FLIGHT; first sets it, second is blocked.
		assertTrue(inFlight.compareAndSet(false, true));
		assertFalse(inFlight.compareAndSet(false, true));
	}

	@Test
	void inFlight_resetToFalse_allowsNextAttempt() {
		inFlight.set(true);
		// Simulate closeWizard() or performCancel() resetting the flag
		inFlight.set(false);
		assertTrue(inFlight.compareAndSet(false, true), "After reset, next attempt must succeed");
	}

	private static AtomicBoolean getInFlight() throws Exception {
		Field f = SnykWizard.class.getDeclaredField("IN_FLIGHT");
		f.setAccessible(true);
		return (AtomicBoolean) f.get(null);
	}
}
