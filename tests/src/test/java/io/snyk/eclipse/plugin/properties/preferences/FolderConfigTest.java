package io.snyk.eclipse.plugin.properties.preferences;

//package io.snyk.languageserver.protocolextension.messageObjects;

import org.junit.jupiter.api.Test;

import io.snyk.languageserver.protocolextension.messageObjects.FolderConfig;

import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class FolderConfigTest {

    @Test
    void testConstructorAndGetters() {
        String folderPath = "/path/to/folder";
        String baseBranch = "main";
        List<String> localBranches = Arrays.asList("feature1", "feature2");
        List<String> additionalParameters = Arrays.asList("-param1", "-param2");

        FolderConfig config = new FolderConfig(folderPath);
        config.setBaseBranch(baseBranch);
        config.setAdditionalParameters(additionalParameters);
        config.setLocalBranches(localBranches);

        assertEquals(folderPath, config.getFolderPath());
        assertEquals(baseBranch, config.getBaseBranch());
        assertEquals(localBranches, config.getLocalBranches());
        assertEquals(additionalParameters, config.getAdditionalParameters());
    }

    @Test
    void testNullListsInConstructor() {
        FolderConfig config = new FolderConfig("path");

        assertNotNull(config.getLocalBranches());
        assertTrue(config.getLocalBranches().isEmpty());
        assertNotNull(config.getAdditionalParameters());
        assertTrue(config.getAdditionalParameters().isEmpty());
    }

    @Test
    void testNullListsInSetters() {
        FolderConfig config = new FolderConfig("path");

        config.setLocalBranches(null);
        config.setAdditionalParameters(null);

        assertNotNull(config.getLocalBranches());
        assertTrue(config.getLocalBranches().isEmpty());
        assertNotNull(config.getAdditionalParameters());
        assertTrue(config.getAdditionalParameters().isEmpty());
    }
}