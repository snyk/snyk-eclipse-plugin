package io.snyk.eclipse.plugin.properties;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.google.gson.Gson;

import io.snyk.languageserver.protocolextension.messageObjects.ConfigSetting;
import io.snyk.languageserver.protocolextension.messageObjects.LspFolderConfig;

class FolderConfigSettingsTest {

	private final Gson gson = new Gson();
	private FolderConfigSettings settings;

	@BeforeEach
	void setUp() {
		settings = new FolderConfigSettings();
		FolderConfigSettings.setInstance(settings);
	}

	@Test
	void singletonReturnsInstance() {
		FolderConfigSettings instance = FolderConfigSettings.getInstance();
		assertNotNull(instance);
		assertSame(instance, FolderConfigSettings.getInstance());
	}

	@Test
	void addFolderConfigAndRetrieve() {
		LspFolderConfig config = createFolderConfig("/home/user/project", "main");

		settings.addFolderConfig(config);

		LspFolderConfig result = settings.getFolderConfig("/home/user/project");
		assertNotNull(result);
		assertEquals("/home/user/project", result.getFolderPath());
	}

	@Test
	void getFolderConfigReturnsEmptyForMissingPath() {
		LspFolderConfig result = settings.getFolderConfig("/nonexistent");

		assertNotNull(result);
		assertNull(result.getSettings());
	}

	@Test
	void addAllReplacesExisting() {
		LspFolderConfig config1 = createFolderConfig("/project1", "main");
		LspFolderConfig config2 = createFolderConfig("/project2", "develop");

		settings.addFolderConfig(config1);
		settings.addAll(List.of(config2));

		// project1 should be gone (addAll clears and replaces)
		LspFolderConfig result1 = settings.getFolderConfig("/project1");
		assertNull(result1.getSettings());

		// project2 should be present
		LspFolderConfig result2 = settings.getFolderConfig("/project2");
		assertNotNull(result2.getSettings());
	}

	@Test
	void getAllReturnsCopy() {
		LspFolderConfig config = createFolderConfig("/project", "main");
		settings.addFolderConfig(config);

		List<LspFolderConfig> all = settings.getAll();
		assertEquals(1, all.size());

		// Modifying returned list should not affect internal state
		all.clear();
		assertEquals(1, settings.getAll().size());
	}

	@Test
	void pathNormalization() {
		LspFolderConfig config = createFolderConfig("/home/user/./project/../project", "main");
		settings.addFolderConfig(config);

		LspFolderConfig result = settings.getFolderConfig("/home/user/project");
		assertNotNull(result);
		assertNotNull(result.getSettings());
	}

	@Test
	void getBaseBranchReturnsValue() {
		LspFolderConfig config = createFolderConfig("/project", "main");
		settings.addFolderConfig(config);

		assertEquals("main", settings.getBaseBranch("/project"));
	}

	@Test
	void getBaseBranchReturnsEmptyForMissingPath() {
		assertEquals("", settings.getBaseBranch("/nonexistent"));
	}

	@Test
	void getPreferredOrgReturnsValue() {
		String json = """
				{
					"folder_path": "/project",
					"settings": {
						"preferred_org": {
							"value": "my-org"
						}
					}
				}
				""";
		LspFolderConfig config = gson.fromJson(json, LspFolderConfig.class);
		settings.addFolderConfig(config);

		assertEquals("my-org", settings.getPreferredOrg("/project"));
	}

	@Test
	void getPreferredOrgReturnsEmptyForMissingPath() {
		assertEquals("", settings.getPreferredOrg("/nonexistent"));
	}

	@Test
	void getLocalBranchesReturnsValue() {
		String json = """
				{
					"folder_path": "/project",
					"settings": {
						"local_branches": {
							"value": ["main", "develop", "feature/test"]
						}
					}
				}
				""";
		LspFolderConfig config = gson.fromJson(json, LspFolderConfig.class);
		settings.addFolderConfig(config);

		List<String> branches = settings.getLocalBranches("/project");
		assertNotNull(branches);
		assertEquals(3, branches.size());
		assertTrue(branches.contains("main"));
		assertTrue(branches.contains("develop"));
		assertTrue(branches.contains("feature/test"));
	}

	@Test
	void getLocalBranchesReturnsEmptyListForMissing() {
		List<String> branches = settings.getLocalBranches("/nonexistent");
		assertNotNull(branches);
		assertTrue(branches.isEmpty());
	}

	@Test
	void getAdditionalParametersReturnsValue() {
		String json = """
				{
					"folder_path": "/project",
					"settings": {
						"additional_parameters": {
							"value": "--all-projects"
						}
					}
				}
				""";
		LspFolderConfig config = gson.fromJson(json, LspFolderConfig.class);
		settings.addFolderConfig(config);

		assertEquals("--all-projects", settings.getAdditionalParameters("/project"));
	}

	@Test
	void getAdditionalParametersReturnsEmptyForMissing() {
		assertEquals("", settings.getAdditionalParameters("/nonexistent"));
	}

	@Test
	void getReferenceFolderPathReturnsValue() {
		String json = """
				{
					"folder_path": "/project",
					"settings": {
						"reference_folder_path": {
							"value": "/ref/path"
						}
					}
				}
				""";
		LspFolderConfig config = gson.fromJson(json, LspFolderConfig.class);
		settings.addFolderConfig(config);

		assertEquals("/ref/path", settings.getReferenceFolderPath("/project"));
	}

	@Test
	void getReferenceFolderPathReturnsEmptyForMissing() {
		assertEquals("", settings.getReferenceFolderPath("/nonexistent"));
	}

	@Test
	void isConfiguredReturnsFalseForMissingPath() {
		assertFalse(settings.isConfigured("/nonexistent"));
	}

	@Test
	void isConfiguredReturnsTrueWhenConfigExists() {
		LspFolderConfig config = createFolderConfig("/project", "main");
		settings.addFolderConfig(config);

		assertTrue(settings.isConfigured("/project"));
	}

	@Test
	void isConfiguredReturnsTrueForEmptySettingsConfig() {
		String json = """
				{
					"folder_path": "/project",
					"settings": {}
				}
				""";
		LspFolderConfig config = gson.fromJson(json, LspFolderConfig.class);
		settings.addFolderConfig(config);

		assertTrue(settings.isConfigured("/project"));
	}

	@Test
	void isConfiguredReturnsFalseForNullPath() {
		assertFalse(settings.isConfigured(null));
	}

	@Test
	void isConfiguredUsesPathNormalization() {
		LspFolderConfig config = createFolderConfig("/home/user/project", "main");
		settings.addFolderConfig(config);

		assertTrue(settings.isConfigured("/home/user/./project/../project"));
	}

	@Test
	void updateFolderConfigStoresConfig() {
		LspFolderConfig config = createFolderConfig("/project", "main");
		settings.addFolderConfig(config);

		LspFolderConfig original = settings.getFolderConfig("/project");
		LspFolderConfig updated = original.withSetting("base_branch", "develop", true);
		settings.updateFolderConfig("/project", updated);

		assertEquals("develop", settings.getBaseBranch("/project"));
	}

	@Test
	void updateFolderConfigNormalizesPath() {
		LspFolderConfig config = createFolderConfig("/home/user/project", "main");
		settings.addFolderConfig(config);

		LspFolderConfig updated = config.withSetting("base_branch", "develop", true);
		settings.updateFolderConfig("/home/user/./project", updated);

		assertEquals("develop", settings.getBaseBranch("/home/user/project"));
	}

	@Test
	void updateFolderConfigCreatesNewEntryIfNotExist() {
		LspFolderConfig config = createFolderConfig("/new-project", "feature");
		settings.updateFolderConfig("/new-project", config);

		assertTrue(settings.isConfigured("/new-project"));
		assertEquals("feature", settings.getBaseBranch("/new-project"));
	}

	@Test
	void isOrgSetByUserReturnsFalseForMissing() {
		assertFalse(settings.isOrgSetByUser("/nonexistent"));
	}

	@Test
	void isOrgSetByUserReturnsTrueWhenSet() {
		String json = """
				{
					"folder_path": "/project",
					"settings": {
						"org_set_by_user": {
							"value": true
						}
					}
				}
				""";
		LspFolderConfig config = gson.fromJson(json, LspFolderConfig.class);
		settings.addFolderConfig(config);

		assertTrue(settings.isOrgSetByUser("/project"));
	}

	@Test
	void isOrgSetByUserReturnsFalseWhenExplicitlyFalse() {
		String json = """
				{
					"folder_path": "/project",
					"settings": {
						"org_set_by_user": {
							"value": false
						}
					}
				}
				""";
		LspFolderConfig config = gson.fromJson(json, LspFolderConfig.class);
		settings.addFolderConfig(config);

		assertFalse(settings.isOrgSetByUser("/project"));
	}

	@Test
	void getAutoDeterminedOrgReturnsValue() {
		String json = """
				{
					"folder_path": "/project",
					"settings": {
						"auto_determined_org": {
							"value": "determined-org"
						}
					}
				}
				""";
		LspFolderConfig config = gson.fromJson(json, LspFolderConfig.class);
		settings.addFolderConfig(config);

		assertEquals("determined-org", settings.getAutoDeterminedOrg("/project"));
	}

	@Test
	void getAutoDeterminedOrgReturnsEmptyForMissing() {
		assertEquals("", settings.getAutoDeterminedOrg("/nonexistent"));
	}

	@Test
	void withSettingIntegration() {
		LspFolderConfig config = createFolderConfig("/project", "main");
		settings.addFolderConfig(config);

		LspFolderConfig original = settings.getFolderConfig("/project");
		LspFolderConfig updated = original.withSetting("base_branch", "develop", true);
		settings.addFolderConfig(updated);

		assertEquals("develop", settings.getBaseBranch("/project"));
	}

	private LspFolderConfig createFolderConfig(String path, String baseBranch) {
		String json = String.format("""
				{
					"folder_path": "%s",
					"settings": {
						"base_branch": {
							"value": "%s",
							"changed": false,
							"source": "cli",
							"origin_scope": "folder",
							"is_locked": false
						}
					}
				}
				""", path, baseBranch);
		return gson.fromJson(json, LspFolderConfig.class);
	}
}
