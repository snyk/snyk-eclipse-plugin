package io.snyk.languageserver;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.lsp4j.WorkspaceFolder;
import org.junit.jupiter.api.Test;

class WorkspaceFolderChangeTrackerTest {

    // -----------------------------------------------------------------------
    // Helpers
    // -----------------------------------------------------------------------

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

    private static WorkspaceFolder folder(String uri) {
        return new WorkspaceFolder(uri, uri.substring(uri.lastIndexOf('/') + 1));
    }

    /** Builds a tracker whose folder source is backed by an AtomicReference map. */
    private static WorkspaceFolderChangeTracker trackerWith(
            AtomicReference<Map<String, WorkspaceFolder>> sourceRef,
            List<List<WorkspaceFolder>> capturedAdded,
            List<List<WorkspaceFolder>> capturedRemoved,
            boolean senderSucceeds) {
        return new WorkspaceFolderChangeTracker(
            sourceRef::get,
            (added, removed) -> {
                capturedAdded.add(new ArrayList<>(added));
                capturedRemoved.add(new ArrayList<>(removed));
                return senderSucceeds;
            });
    }

    // -----------------------------------------------------------------------
    // affectsProjectSet
    // -----------------------------------------------------------------------

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

    // -----------------------------------------------------------------------
    // recomputeAndNotify — unit tests
    // -----------------------------------------------------------------------

    @Test
    void recompute_noChange_doesNotSend() {
        WorkspaceFolder a = folder("file:///a");
        AtomicReference<Map<String, WorkspaceFolder>> source =
            new AtomicReference<>(Map.of("file:///a", a));
        List<List<WorkspaceFolder>> sentAdded = new ArrayList<>();
        List<List<WorkspaceFolder>> sentRemoved = new ArrayList<>();

        WorkspaceFolderChangeTracker tracker = trackerWith(source, sentAdded, sentRemoved, true);

        tracker.recomputeAndNotify();

        assertTrue(sentAdded.isEmpty(), "no send when nothing changed");
    }

    @Test
    void recompute_projectAdded_sendsAddedListAndUpdatesState() {
        WorkspaceFolder a = folder("file:///a");
        WorkspaceFolder b = folder("file:///b");
        AtomicReference<Map<String, WorkspaceFolder>> source =
            new AtomicReference<>(Map.of("file:///a", a));
        List<List<WorkspaceFolder>> sentAdded = new ArrayList<>();
        List<List<WorkspaceFolder>> sentRemoved = new ArrayList<>();

        WorkspaceFolderChangeTracker tracker = trackerWith(source, sentAdded, sentRemoved, true);

        source.set(Map.of("file:///a", a, "file:///b", b));
        tracker.recomputeAndNotify();

        assertEquals(1, sentAdded.size());
        assertEquals(List.of(b), sentAdded.get(0));
        assertEquals(List.of(), sentRemoved.get(0));

        // State updated: second recompute with same source must not send again.
        sentAdded.clear();
        tracker.recomputeAndNotify();
        assertTrue(sentAdded.isEmpty(), "state should be updated after successful send");
    }

    @Test
    void recompute_projectRemoved_sendsRemovedListAndUpdatesState() {
        WorkspaceFolder a = folder("file:///a");
        WorkspaceFolder b = folder("file:///b");
        AtomicReference<Map<String, WorkspaceFolder>> source =
            new AtomicReference<>(Map.of("file:///a", a, "file:///b", b));
        List<List<WorkspaceFolder>> sentAdded = new ArrayList<>();
        List<List<WorkspaceFolder>> sentRemoved = new ArrayList<>();

        WorkspaceFolderChangeTracker tracker = trackerWith(source, sentAdded, sentRemoved, true);

        source.set(Map.of("file:///a", a));
        tracker.recomputeAndNotify();

        assertEquals(1, sentRemoved.size());
        assertEquals(List.of(b), sentRemoved.get(0));
        assertEquals(List.of(), sentAdded.get(0));

        sentRemoved.clear();
        tracker.recomputeAndNotify();
        assertTrue(sentRemoved.isEmpty(), "state should be updated after successful send");
    }

    @Test
    void recompute_sendFails_stateNotUpdatedSoNextCallRetriesSend() {
        WorkspaceFolder a = folder("file:///a");
        WorkspaceFolder b = folder("file:///b");
        AtomicReference<Map<String, WorkspaceFolder>> source =
            new AtomicReference<>(Map.of("file:///a", a));
        List<List<WorkspaceFolder>> sentAdded = new ArrayList<>();
        AtomicBoolean shouldSucceed = new AtomicBoolean(false);

        WorkspaceFolderChangeTracker tracker = new WorkspaceFolderChangeTracker(
            source::get,
            (added, removed) -> {
                sentAdded.add(new ArrayList<>(added));
                return shouldSucceed.get();
            });

        source.set(Map.of("file:///a", a, "file:///b", b));
        tracker.recomputeAndNotify(); // send fails

        assertEquals(1, sentAdded.size());
        sentAdded.clear();

        // State was NOT updated, so another recompute must send again.
        tracker.recomputeAndNotify();
        assertEquals(1, sentAdded.size(), "should retry when previous send failed");
        assertEquals(List.of(b), sentAdded.get(0));
    }

    @Test
    void recompute_simultaneousAddAndRemove_sendsBothLists() {
        WorkspaceFolder a = folder("file:///a");
        WorkspaceFolder b = folder("file:///b");
        AtomicReference<Map<String, WorkspaceFolder>> source =
            new AtomicReference<>(Map.of("file:///a", a));
        List<List<WorkspaceFolder>> sentAdded = new ArrayList<>();
        List<List<WorkspaceFolder>> sentRemoved = new ArrayList<>();

        WorkspaceFolderChangeTracker tracker = trackerWith(source, sentAdded, sentRemoved, true);

        // Replace a with b.
        source.set(Map.of("file:///b", b));
        tracker.recomputeAndNotify();

        assertEquals(List.of(b), sentAdded.get(0));
        assertEquals(List.of(a), sentRemoved.get(0));
    }

    // -----------------------------------------------------------------------
    // recomputeAndNotify — via resourceChanged (integration-esque)
    // -----------------------------------------------------------------------

    @Test
    void resourceChanged_addedDelta_sendsNotificationAndUpdatesState() {
        WorkspaceFolder a = folder("file:///a");
        WorkspaceFolder b = folder("file:///b");
        AtomicReference<Map<String, WorkspaceFolder>> source =
            new AtomicReference<>(Map.of("file:///a", a));
        List<List<WorkspaceFolder>> sentAdded = new ArrayList<>();
        List<List<WorkspaceFolder>> sentRemoved = new ArrayList<>();

        WorkspaceFolderChangeTracker tracker = trackerWith(source, sentAdded, sentRemoved, true);
        source.set(Map.of("file:///a", a, "file:///b", b));

        IResourceDelta addedDelta = rootDelta(child(IResourceDelta.ADDED, 0));
        IResourceChangeEvent event = mock(IResourceChangeEvent.class);
        when(event.getDelta()).thenReturn(addedDelta);
        tracker.resourceChanged(event);

        assertEquals(1, sentAdded.size());
        assertEquals(List.of(b), sentAdded.get(0));
        assertEquals(List.of(), sentRemoved.get(0));

        // Second event with same source — state already updated, nothing sent.
        sentAdded.clear();
        tracker.resourceChanged(event);
        assertTrue(sentAdded.isEmpty(), "state updated after first event");
    }

    @Test
    void resourceChanged_removedDelta_sendsNotificationAndUpdatesState() {
        WorkspaceFolder a = folder("file:///a");
        WorkspaceFolder b = folder("file:///b");
        AtomicReference<Map<String, WorkspaceFolder>> source =
            new AtomicReference<>(Map.of("file:///a", a, "file:///b", b));
        List<List<WorkspaceFolder>> sentAdded = new ArrayList<>();
        List<List<WorkspaceFolder>> sentRemoved = new ArrayList<>();

        WorkspaceFolderChangeTracker tracker = trackerWith(source, sentAdded, sentRemoved, true);
        source.set(Map.of("file:///a", a));

        IResourceDelta removedDelta = rootDelta(child(IResourceDelta.REMOVED, 0));
        IResourceChangeEvent event = mock(IResourceChangeEvent.class);
        when(event.getDelta()).thenReturn(removedDelta);
        tracker.resourceChanged(event);

        assertEquals(1, sentRemoved.size());
        assertEquals(List.of(b), sentRemoved.get(0));
        assertEquals(List.of(), sentAdded.get(0));

        sentRemoved.clear();
        tracker.resourceChanged(event);
        assertTrue(sentRemoved.isEmpty(), "state updated after removal event");
    }

    @Test
    void resourceChanged_unrelatedDelta_doesNotSend() {
        WorkspaceFolder a = folder("file:///a");
        AtomicReference<Map<String, WorkspaceFolder>> source =
            new AtomicReference<>(Map.of("file:///a", a));
        List<List<WorkspaceFolder>> sentAdded = new ArrayList<>();
        List<List<WorkspaceFolder>> sentRemoved = new ArrayList<>();

        WorkspaceFolderChangeTracker tracker = trackerWith(source, sentAdded, sentRemoved, true);

        // Source has a new project, but the delta is a file save — should be ignored.
        WorkspaceFolder b = folder("file:///b");
        source.set(Map.of("file:///a", a, "file:///b", b));

        IResourceDelta unrelatedDelta = rootDelta(child(IResourceDelta.CHANGED, IResourceDelta.CONTENT));
        IResourceChangeEvent event = mock(IResourceChangeEvent.class);
        when(event.getDelta()).thenReturn(unrelatedDelta);
        tracker.resourceChanged(event);

        assertTrue(sentAdded.isEmpty(), "unrelated delta must not trigger notification");
    }
}
