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
	void canFinish_returnsFalse_whenInFlight() {
		inFlight.set(true);
		SnykWizard wizard = new SnykWizard();
		assertFalse(wizard.canFinish(), "canFinish() must return false while auth is in flight");
	}

	@Test
	void canFinish_returnsTrue_whenNotInFlight() {
		inFlight.set(false);
		SnykWizard wizard = new SnykWizard();
		assertTrue(wizard.canFinish(), "canFinish() must return true when no auth is in flight");
	}

	@Test
	void performFinish_returnsFalse_whenInFlight() {
		inFlight.set(true);
		SnykWizard wizard = new SnykWizard();
		assertFalse(wizard.performFinish(), "performFinish() must be blocked while auth is in flight");
		assertTrue(inFlight.get(), "IN_FLIGHT must remain true — guard must not be reset on early return");
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
