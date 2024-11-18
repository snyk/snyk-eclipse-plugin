package io.snyk.eclipse.plugin.views.snyktoolview;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

import java.util.Arrays;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.TreeNode;

class BaseTreeNodeTest {
    private BaseTreeNode node;
    private ImageDescriptor mockImageDescriptor;

    @BeforeEach
    void setUp() {
        node = new BaseTreeNode("root");
        mockImageDescriptor = mock(ImageDescriptor.class);
    }

    @Test
    void testConstructorWithString() {
        assertEquals("root", node.getText());
        assertEquals("root", node.getValue());
    }

    @Test
    void testConstructorWithNonString() {
        Integer value = 42;
        BaseTreeNode intNode = new BaseTreeNode(value);
        assertEquals("", intNode.getText());
        assertEquals(value, intNode.getValue());
    }

    @Test
    void testSetValue() {
        String newValue = "newValue";
        node.setValue(newValue);
        assertEquals(newValue, node.getValue());
    }

    @Test
    void testAddChildToEmptyNode() {
        BaseTreeNode child = new BaseTreeNode("child1");
        node.addChild(child);
        
        TreeNode[] children = node.getChildren();
        assertNotNull(children);
        assertEquals(1, children.length);
        assertEquals(child, children[0]);
    }

    @Test
    void testAddChildToNodeWithExistingChildren() {
        BaseTreeNode child1 = new BaseTreeNode("child1");
        BaseTreeNode child2 = new BaseTreeNode("child2");
        
        node.addChild(child1);
        node.addChild(child2);
        
        TreeNode[] children = node.getChildren();
        assertNotNull(children);
        assertEquals(2, children.length);
        assertEquals(child1, children[0]);
        assertEquals(child2, children[1]);
    }

    @Test
    void testRemoveChildren() {
        BaseTreeNode child = new BaseTreeNode("child");
        node.addChild(child);
        
        node.removeChildren();
        
        TreeNode[] children = node.getChildren();
        assertNotNull(children);
        assertEquals(0, children.length);
    }

    @Test
    void testSetAndGetImageDescriptor() {
        node.setImageDescriptor(mockImageDescriptor);
        assertEquals(mockImageDescriptor, node.getImageDescriptor());
    }

    @Test
    void testSetAndGetText() {
        String newText = "newText";
        node.setText(newText);
        assertEquals(newText, node.getText());
    }

    @Test
    void testToString() {
        assertEquals("root", node.toString());
        
        Integer value = 42;
        BaseTreeNode intNode = new BaseTreeNode(value);
        assertEquals("42", intNode.toString());
    }

    @Test
    void testReset() {
        // Setup initial state
        node.setText("someText");
        node.setImageDescriptor(mockImageDescriptor);
        node.addChild(new BaseTreeNode("child"));
        
        // Reset the node
        node.reset();
        
        // Verify all fields are reset
        assertNull(node.getValue());
        assertEquals("", node.getText());
        assertNull(node.getImageDescriptor());
        assertEquals(0, node.getChildren().length);
    }

    @Test
    void testAddMultipleChildren() {
        BaseTreeNode[] children = new BaseTreeNode[] {
            new BaseTreeNode("child1"),
            new BaseTreeNode("child2"),
            new BaseTreeNode("child3")
        };
        
        Arrays.stream(children).forEach(node::addChild);
        
        TreeNode[] resultChildren = node.getChildren();
        assertNotNull(resultChildren);
        assertEquals(children.length, resultChildren.length);
        
        for (int i = 0; i < children.length; i++) {
            assertEquals(children[i], resultChildren[i]);
        }
    }

    @Test
    void testAddChildAfterRemovingChildren() {
        BaseTreeNode child1 = new BaseTreeNode("child1");
        node.addChild(child1);
        node.removeChildren();
        
        BaseTreeNode child2 = new BaseTreeNode("child2");
        node.addChild(child2);
        
        TreeNode[] children = node.getChildren();
        assertNotNull(children);
        assertEquals(1, children.length);
        assertEquals(child2, children[0]);
    }
}