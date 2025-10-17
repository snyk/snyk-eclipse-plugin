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

    @Test
    void testAutoDeterminedOrg() {
        FolderConfig config = new FolderConfig("path");

        // Test initial state
        assertNull(config.getAutoDeterminedOrg());

        // Test setting autoDeterminedOrg
        String autoOrg = "auto-determined-org";
        config.setAutoDeterminedOrg(autoOrg);
        assertEquals(autoOrg, config.getAutoDeterminedOrg());

        // Test setting null
        config.setAutoDeterminedOrg(null);
        assertNull(config.getAutoDeterminedOrg());
    }

    @Test
    void testOrgSetByUser() {
        FolderConfig config = new FolderConfig("path");

        // Test initial state
        assertFalse(config.isOrgSetByUser());

        // Test setting orgSetByUser to true
        config.setOrgSetByUser(true);
        assertTrue(config.isOrgSetByUser());

        // Test setting orgSetByUser to false
        config.setOrgSetByUser(false);
        assertFalse(config.isOrgSetByUser());
    }

    @Test
    void testPreferredOrg() {
        FolderConfig config = new FolderConfig("path");

        // Test initial state
        assertNull(config.getPreferredOrg());

        // Test setting preferredOrg
        String preferredOrg = "preferred-org";
        config.setPreferredOrg(preferredOrg);
        assertEquals(preferredOrg, config.getPreferredOrg());

        // Test setting null
        config.setPreferredOrg(null);
        assertNull(config.getPreferredOrg());
    }

    @Test
    void testGlobalOrgFallback() {
        FolderConfig config = new FolderConfig("path");

        // Test that empty preferred org can be set
        config.setPreferredOrg("");
        assertEquals("", config.getPreferredOrg());

        // Test that null preferred org can be set
        config.setPreferredOrg(null);
        assertNull(config.getPreferredOrg());

        // Test that whitespace-only preferred org can be set
        config.setPreferredOrg("   ");
        assertEquals("   ", config.getPreferredOrg());
    }
}
