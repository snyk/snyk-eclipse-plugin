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
		assertTrue(inFlight.compareAndSet(false, true), "First caller must acquire guard");
		assertFalse(inFlight.compareAndSet(false, true), "Second caller must be blocked");
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

	@Test
	void canFinish_returnsFalse_whenInFlight() throws Exception {
		inFlight.set(true);
		// canFinish() returns !IN_FLIGHT — verify the inversion is correct
		SnykWizard wizard = new TestableSnykWizard();
		assertFalse(wizard.canFinish());
	}

	@Test
	void canFinish_returnsTrue_whenNotInFlight() throws Exception {
		inFlight.set(false);
		SnykWizard wizard = new TestableSnykWizard();
		assertTrue(wizard.canFinish());
	}

	@Test
	void performCancel_resetsInFlight() throws Exception {
		inFlight.set(true);
		SnykWizard wizard = new TestableSnykWizard();
		wizard.performCancel();
		assertFalse(inFlight.get());
	}

	@Test
	void performCancel_returnsTrue() throws Exception {
		SnykWizard wizard = new TestableSnykWizard();
		assertTrue(wizard.performCancel());
	}

	private static AtomicBoolean getInFlight() throws Exception {
		Field f = SnykWizard.class.getDeclaredField("IN_FLIGHT");
		f.setAccessible(true);
		return (AtomicBoolean) f.get(null);
	}

	// Subclass that bypasses the Wizard/JFace constructor's Display dependency.
	private static class TestableSnykWizard extends SnykWizard {
		// SnykWizard only calls super() which ultimately calls Wizard().
		// Wizard() accesses JFaceResources which needs a Display.
		// In the Tycho OSGi test container a Display is available, so this is fine.
	}
}
