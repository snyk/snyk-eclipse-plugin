package io.snyk.languageserver.protocolextension.messageObjects;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Map;

import org.junit.jupiter.api.Test;

import com.google.gson.Gson;

class LspFolderConfigTest {

	private final Gson gson = new Gson();

	@Test
	void deserializesFolderPathFromSnakeCase() {
		String json = """
				{
					"folder_path": "/home/user/project",
					"settings": {}
				}
				""";

		LspFolderConfig config = gson.fromJson(json, LspFolderConfig.class);

		assertEquals("/home/user/project", config.getFolderPath());
		assertNotNull(config.getSettings());
		assertTrue(config.getSettings().isEmpty());
	}

	@Test
	void deserializesWithSettings() {
		String json = """
				{
					"folder_path": "/project",
					"settings": {
						"base_branch": {
							"value": "main",
							"changed": false,
							"source": "cli",
							"origin_scope": "folder",
							"is_locked": true
						}
					}
				}
				""";

		LspFolderConfig config = gson.fromJson(json, LspFolderConfig.class);

		assertEquals("/project", config.getFolderPath());
		assertEquals(1, config.getSettings().size());

		ConfigSetting baseBranch = config.getSettings().get("base_branch");
		assertNotNull(baseBranch);
		assertEquals("main", baseBranch.getValue());
		assertFalse(baseBranch.getChanged());
		assertEquals("cli", baseBranch.getSource());
		assertEquals("folder", baseBranch.getOriginScope());
		assertTrue(baseBranch.getIsLocked());
	}

	@Test
	void deserializesWithNullSettings() {
		String json = """
				{
					"folder_path": "/project"
				}
				""";

		LspFolderConfig config = gson.fromJson(json, LspFolderConfig.class);

		assertEquals("/project", config.getFolderPath());
		assertNull(config.getSettings());
	}

	@Test
	void withSettingPreservesLockMetadata() {
		String json = """
				{
					"folder_path": "/project",
					"settings": {
						"base_branch": {
							"value": "main",
							"changed": false,
							"source": "cli",
							"origin_scope": "folder",
							"is_locked": true
						}
					}
				}
				""";

		LspFolderConfig original = gson.fromJson(json, LspFolderConfig.class);
		LspFolderConfig updated = original.withSetting("base_branch", "develop", true);

		// Original is not mutated
		assertEquals("main", original.getSettings().get("base_branch").getValue());
		assertFalse(original.getSettings().get("base_branch").getChanged());

		// Updated has new value and changed flag
		assertEquals("develop", updated.getSettings().get("base_branch").getValue());
		assertTrue(updated.getSettings().get("base_branch").getChanged());

		// Lock metadata is preserved
		assertEquals("cli", updated.getSettings().get("base_branch").getSource());
		assertEquals("folder", updated.getSettings().get("base_branch").getOriginScope());
		assertTrue(updated.getSettings().get("base_branch").getIsLocked());

		// Folder path is preserved
		assertEquals("/project", updated.getFolderPath());
	}

	@Test
	void withSettingCreatesSettingForNewKey() {
		String json = """
				{
					"folder_path": "/project",
					"settings": {}
				}
				""";

		LspFolderConfig original = gson.fromJson(json, LspFolderConfig.class);
		LspFolderConfig updated = original.withSetting("preferred_org", "my-org", true);

		// Original not mutated
		assertFalse(original.getSettings().containsKey("preferred_org"));

		// New key created
		ConfigSetting setting = updated.getSettings().get("preferred_org");
		assertNotNull(setting);
		assertEquals("my-org", setting.getValue());
		assertTrue(setting.getChanged());
		assertNull(setting.getSource());
		assertNull(setting.getOriginScope());
		assertNull(setting.getIsLocked());
	}

	@Test
	void withSettingReturnsNewInstance() {
		String json = """
				{
					"folder_path": "/project",
					"settings": {
						"key": { "value": "val" }
					}
				}
				""";

		LspFolderConfig original = gson.fromJson(json, LspFolderConfig.class);
		LspFolderConfig updated = original.withSetting("key", "newVal", true);

		assertNotSame(original, updated);
	}

	@Test
	void withSettingNullValueForResetToDefault() {
		String json = """
				{
					"folder_path": "/project",
					"settings": {
						"base_branch": {
							"value": "main",
							"changed": false,
							"source": "cli",
							"origin_scope": "folder",
							"is_locked": false
						}
					}
				}
				""";

		LspFolderConfig original = gson.fromJson(json, LspFolderConfig.class);
		LspFolderConfig reset = original.withSetting("base_branch", null, true);

		ConfigSetting setting = reset.getSettings().get("base_branch");
		assertNull(setting.getValue());
		assertTrue(setting.getChanged());
		assertEquals("cli", setting.getSource());
		assertEquals("folder", setting.getOriginScope());
	}

	@Test
	void withSettingHandlesNullSettings() {
		String json = """
				{
					"folder_path": "/project"
				}
				""";

		LspFolderConfig original = gson.fromJson(json, LspFolderConfig.class);
		LspFolderConfig updated = original.withSetting("key", "value", false);

		assertNotNull(updated.getSettings());
		assertEquals("value", updated.getSettings().get("key").getValue());
	}
}
