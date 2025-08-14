package io.snyk.eclipse.plugin.views.snyktoolview.providers;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.any;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Display;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import io.snyk.eclipse.plugin.views.snyktoolview.BaseTreeNode;

class TreeLabelProviderTest {
    private TreeLabelProvider labelProvider;
    private BaseTreeNode mockNode;
    private ImageDescriptor mockImageDescriptor;
    private Image mockImage;
    private Display mockDisplay;

    @BeforeEach
    void setUp() {
        labelProvider = new TreeLabelProvider();
        mockNode = mock(BaseTreeNode.class);
        mockImageDescriptor = mock(ImageDescriptor.class);
        mockImage = mock(Image.class);
        mockDisplay = mock(Display.class);
    }

    @AfterEach
    void tearDown() {
        labelProvider.dispose();
    }

    @Test
    void testGetTextWithBaseTreeNode() {
        String expectedText = "Test Node";
        when(mockNode.getText()).thenReturn(expectedText);
        
        String actualText = labelProvider.getText(mockNode);
        
        assertEquals(expectedText, actualText);
    }

    @Test
    void testGetTextWithNonTreeNode() {
        String testObject = "Test String";
        
        String actualText = labelProvider.getText(testObject);
        
        assertEquals(testObject, actualText);
    }

    @Test
    void testGetImageWithNullImageDescriptor() {
        when(mockNode.getImageDescriptor()).thenReturn(null);
        
        Image result = labelProvider.getImage(mockNode);
        
        assertNull(result);
    }

    @Test
    void testGetImageWithNonTreeNode() {
        Image result = labelProvider.getImage("Not a tree node");
        
        assertNull(result);
    }

    @Test
    void testGetImageCachesImages() {
        try (MockedStatic<Display> displayMock = Mockito.mockStatic(Display.class)) {
            displayMock.when(Display::getDefault).thenReturn(mockDisplay);
            when(mockNode.getImageDescriptor()).thenReturn(mockImageDescriptor);
            when(mockImageDescriptor.createResource(mockDisplay)).thenReturn(mockImage);
            when(mockImage.isDisposed()).thenReturn(false);
            
            // First call should create the image
            Image firstResult = labelProvider.getImage(mockNode);
            assertNotNull(firstResult);
            
            // Second call should return cached image
            Image secondResult = labelProvider.getImage(mockNode);
            assertSame(firstResult, secondResult);
            
            // Verify createResource was called only once
            verify(mockImageDescriptor, times(1)).createResource(mockDisplay);
        }
    }

    @Test
    void testGetImageRecreatesDisposedImage() {
        try (MockedStatic<Display> displayMock = Mockito.mockStatic(Display.class)) {
            displayMock.when(Display::getDefault).thenReturn(mockDisplay);
            when(mockNode.getImageDescriptor()).thenReturn(mockImageDescriptor);
            when(mockImageDescriptor.createResource(mockDisplay)).thenReturn(mockImage);
            
            // First call - image is not disposed
            when(mockImage.isDisposed()).thenReturn(false);
            Image firstResult = labelProvider.getImage(mockNode);
            assertNotNull(firstResult);
            
            // Second call - image is disposed
            when(mockImage.isDisposed()).thenReturn(true);
            Image secondMockImage = mock(Image.class);
            when(mockImageDescriptor.createResource(mockDisplay)).thenReturn(secondMockImage);
            
            Image secondResult = labelProvider.getImage(mockNode);
            assertNotNull(secondResult);
            
            // Verify createResource was called twice
            verify(mockImageDescriptor, times(2)).createResource(mockDisplay);
        }
    }

    @Test
    void testConcurrentGetImageThreadSafety() throws InterruptedException {
        try (MockedStatic<Display> displayMock = Mockito.mockStatic(Display.class)) {
            displayMock.when(Display::getDefault).thenReturn(mockDisplay);
            when(mockNode.getImageDescriptor()).thenReturn(mockImageDescriptor);
            when(mockImage.isDisposed()).thenReturn(false);
            
            AtomicInteger createResourceCallCount = new AtomicInteger(0);
            when(mockImageDescriptor.createResource(mockDisplay)).thenAnswer(invocation -> {
                createResourceCallCount.incrementAndGet();
                Thread.sleep(10); // Simulate some processing time
                return mockImage;
            });
            
            int threadCount = 10;
            ExecutorService executor = Executors.newFixedThreadPool(threadCount);
            CountDownLatch startLatch = new CountDownLatch(1);
            CountDownLatch completeLatch = new CountDownLatch(threadCount);
            
            for (int i = 0; i < threadCount; i++) {
                executor.submit(() -> {
                    try {
                        startLatch.await(); // Wait for all threads to be ready
                        labelProvider.getImage(mockNode);
                    } catch (InterruptedException e) {
                        Thread.currentThread().interrupt();
                    } finally {
                        completeLatch.countDown();
                    }
                });
            }
            
            // Start all threads at the same time
            startLatch.countDown();
            
            // Wait for all threads to complete
            assertTrue(completeLatch.await(5, TimeUnit.SECONDS), "Threads did not complete in time");
            executor.shutdown();
            
            // Even with concurrent access, createResource should be called only once
            // In test environment, this might be 0 if Display is not available
            assertTrue(createResourceCallCount.get() <= 1, 
                "createResource was called more than once: " + createResourceCallCount.get());
        }
    }

    @Test
    void testDisposeReleasesAllImages() {
        try (MockedStatic<Display> displayMock = Mockito.mockStatic(Display.class)) {
            displayMock.when(Display::getDefault).thenReturn(mockDisplay);
            
            // Create multiple nodes with different descriptors
            BaseTreeNode node1 = mock(BaseTreeNode.class);
            BaseTreeNode node2 = mock(BaseTreeNode.class);
            ImageDescriptor descriptor1 = mock(ImageDescriptor.class);
            ImageDescriptor descriptor2 = mock(ImageDescriptor.class);
            Image image1 = mock(Image.class);
            Image image2 = mock(Image.class);
            
            when(node1.getImageDescriptor()).thenReturn(descriptor1);
            when(node2.getImageDescriptor()).thenReturn(descriptor2);
            when(descriptor1.createResource(mockDisplay)).thenReturn(image1);
            when(descriptor2.createResource(mockDisplay)).thenReturn(image2);
            when(image1.isDisposed()).thenReturn(false);
            when(image2.isDisposed()).thenReturn(false);
            
            // Get images to populate cache
            labelProvider.getImage(node1);
            labelProvider.getImage(node2);
            
            // Dispose the label provider
            labelProvider.dispose();
            
            // Verify all images were destroyed
            verify(descriptor1).destroyResource(image1);
            verify(descriptor2).destroyResource(image2);
        }
    }

    @Test
    void testIsLabelPropertyAlwaysReturnsFalse() {
        boolean result = labelProvider.isLabelProperty(mockNode, "anyProperty");
        assertEquals(false, result);
    }
}


