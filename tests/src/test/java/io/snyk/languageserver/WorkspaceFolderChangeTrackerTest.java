package io.snyk.languageserver;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.eclipse.core.resources.IResourceDelta;
import org.junit.jupiter.api.Test;

class WorkspaceFolderChangeTrackerTest {

    private static IResourceDelta rootDelta(IResourceDelta... children) {
        IResourceDelta root = mock(IResourceDelta.class);
        when(root.getAffectedChildren()).thenReturn(children);
        return root;
    }

    private static IResourceDelta child(int kind, int flags) {
        IResourceDelta d = mock(IResourceDelta.class);
        when(d.getKind()).thenReturn(kind);
        when(d.getFlags()).thenReturn(flags);
        return d;
    }

    @Test
    void nullDelta_returnsFalse() {
        assertFalse(WorkspaceFolderChangeTracker.affectsProjectSet(null));
    }

    @Test
    void noAffectedChildren_returnsFalse() {
        assertFalse(WorkspaceFolderChangeTracker.affectsProjectSet(rootDelta()));
    }

    @Test
    void addedProject_returnsTrue() {
        assertTrue(WorkspaceFolderChangeTracker.affectsProjectSet(
            rootDelta(child(IResourceDelta.ADDED, 0))));
    }

    @Test
    void removedProject_returnsTrue() {
        assertTrue(WorkspaceFolderChangeTracker.affectsProjectSet(
            rootDelta(child(IResourceDelta.REMOVED, 0))));
    }

    @Test
    void openFlagSet_returnsTrue() {
        // Covers both project-open and project-close: IResourceDelta.OPEN fires
        // whenever the open state changes, not only when a project is opened.
        assertTrue(WorkspaceFolderChangeTracker.affectsProjectSet(
            rootDelta(child(IResourceDelta.CHANGED, IResourceDelta.OPEN))));
    }

    @Test
    void closeFlagIsOpenFlag_returnsTrue() {
        // Closing a project sets IResourceDelta.OPEN — it is the same bit used
        // for open. This test makes the counterintuitive mapping explicit.
        assertTrue(WorkspaceFolderChangeTracker.affectsProjectSet(
            rootDelta(child(IResourceDelta.CHANGED, IResourceDelta.OPEN))));
    }

    @Test
    void unrelatedChange_returnsFalse() {
        // File save / compilation — CHANGED with no OPEN flag
        assertFalse(WorkspaceFolderChangeTracker.affectsProjectSet(
            rootDelta(child(IResourceDelta.CHANGED, IResourceDelta.CONTENT))));
    }

    @Test
    void mixedChildren_onlyUnrelatedChange_returnsFalse() {
        assertFalse(WorkspaceFolderChangeTracker.affectsProjectSet(
            rootDelta(
                child(IResourceDelta.CHANGED, IResourceDelta.MARKERS),
                child(IResourceDelta.CHANGED, IResourceDelta.CONTENT))));
    }

    @Test
    void mixedChildren_oneAdded_returnsTrue() {
        assertTrue(WorkspaceFolderChangeTracker.affectsProjectSet(
            rootDelta(
                child(IResourceDelta.CHANGED, IResourceDelta.CONTENT),
                child(IResourceDelta.ADDED, 0))));
    }
}
