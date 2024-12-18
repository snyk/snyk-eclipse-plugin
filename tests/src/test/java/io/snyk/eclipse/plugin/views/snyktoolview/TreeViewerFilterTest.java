package io.snyk.eclipse.plugin.views.snyktoolview;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.function.Predicate;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.TreeViewer;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import io.snyk.languageserver.protocolextension.messageObjects.scanResults.Issue;

class TreeViewerFilterTest {

    private TreeViewerFilter filter;
    private TreeViewer mockViewer;
    private ITreeContentProvider mockContentProvider;

    @BeforeEach
    void setUp() {
        filter = new TreeViewerFilter();
        mockViewer = mock(TreeViewer.class);
        mockContentProvider = mock(ITreeContentProvider.class);
        when(mockViewer.getContentProvider()).thenReturn(mockContentProvider);
    }

    @Test
    void testSelect_IssueTreeNodeWithNoFilters() {
        IssueTreeNode issueNode = mock(IssueTreeNode.class);
        Issue issue = mock(Issue.class);
        when(issueNode.getIssue()).thenReturn(issue);

        assertTrue(filter.select(mockViewer, null, issueNode));
    }

    @Test
    void testSelect_IssueTreeNodeWithMatchingFilter() {
        IssueTreeNode issueNode = mock(IssueTreeNode.class);
        Issue issue = mock(Issue.class);
        when(issueNode.getIssue()).thenReturn(issue);

        Predicate<Issue> predicate = i -> true; // Always matches
        filter.putFilterPredicate("test", predicate);

        assertTrue(filter.select(mockViewer, null, issueNode));
    }

    @Test
    void testSelect_IssueTreeNodeWithNonMatchingFilter() {
        IssueTreeNode issueNode = mock(IssueTreeNode.class);
        Issue issue = mock(Issue.class);
        when(issueNode.getIssue()).thenReturn(issue);

        Predicate<Issue> predicate = i -> false; // Never matches
        filter.putFilterPredicate("test", predicate);

        assertFalse(filter.select(mockViewer, null, issueNode));
    }

    @Test
    void testSelect_FileTreeNodeWithVisibleChildren() {
        FileTreeNode fileNode = mock(FileTreeNode.class);
        IssueTreeNode visibleChild = mock(IssueTreeNode.class);
        
        when(mockContentProvider.getChildren(fileNode)).thenReturn(new Object[] { visibleChild });
        
        Issue issue = mock(Issue.class);
        when(visibleChild.getIssue()).thenReturn(issue);

        assertTrue(filter.select(mockViewer, null, fileNode));
    }

    @Test
    void testSelect_FileTreeNodeWithNoVisibleChildren() {
        FileTreeNode fileNode = mock(FileTreeNode.class);
        
        when(mockContentProvider.getChildren(fileNode)).thenReturn(new Object[] {});

        assertFalse(filter.select(mockViewer, null, fileNode));
    }

    @Test
    void testRemoveFilterPredicate() {
        Predicate<Issue> predicate = i -> false;
        filter.putFilterPredicate("test", predicate);

        assertTrue(filter.select(mockViewer, null, mock(IssueTreeNode.class)));

        filter.removeFilterPredicate("test");

        assertTrue(filter.select(mockViewer, null, mock(IssueTreeNode.class)));
    }
}