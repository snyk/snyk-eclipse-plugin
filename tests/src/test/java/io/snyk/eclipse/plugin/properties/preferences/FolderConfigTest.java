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

        FolderConfig config = new FolderConfig(folderPath, baseBranch, localBranches, additionalParameters);

        assertEquals(folderPath, config.getFolderPath());
        assertEquals(baseBranch, config.getBaseBranch());
        assertEquals(localBranches, config.getLocalBranches());
        assertEquals(additionalParameters, config.getAdditionalParameters());
    }

    @Test
    void testSetters() {
        FolderConfig config = new FolderConfig("", "", null, null);

        String folderPath = "/new/path";
        String baseBranch = "develop";
        List<String> localBranches = Arrays.asList("branch1", "branch2");
        List<String> additionalParameters = Arrays.asList("-param3", "-param4");

        config.setFolderPath(folderPath);
        config.setBaseBranch(baseBranch);
        config.setLocalBranches(localBranches);
        config.setAdditionalParameters(additionalParameters);

        assertEquals(folderPath, config.getFolderPath());
        assertEquals(baseBranch, config.getBaseBranch());
        assertEquals(localBranches, config.getLocalBranches());
        assertEquals(additionalParameters, config.getAdditionalParameters());
    }

    @Test
    void testNullListsInConstructor() {
        FolderConfig config = new FolderConfig("path", "branch", null, null);

        assertNotNull(config.getLocalBranches());
        assertTrue(config.getLocalBranches().isEmpty());
        assertNotNull(config.getAdditionalParameters());
        assertTrue(config.getAdditionalParameters().isEmpty());
    }

    @Test
    void testNullListsInSetters() {
        FolderConfig config = new FolderConfig("path", "branch", Arrays.asList("branch1"), Arrays.asList("-param1"));

        config.setLocalBranches(null);
        config.setAdditionalParameters(null);

        assertNotNull(config.getLocalBranches());
        assertTrue(config.getLocalBranches().isEmpty());
        assertNotNull(config.getAdditionalParameters());
        assertTrue(config.getAdditionalParameters().isEmpty());
    }
}