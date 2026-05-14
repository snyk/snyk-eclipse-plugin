package io.snyk.languageserver.protocolextension.messageObjects;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import com.google.gson.Gson;

class LspConfigurationParamTest {

	private final Gson gson = new Gson();

	@Test
	void deserializesGlobalOnly() {
		String json = """
				{
					"settings": {
						"endpoint": {
							"value": "https://api.snyk.io",
							"changed": false,
							"source": "default",
							"originScope": "machine",
							"isLocked": false
						}
					}
				}
				""";

		LspConfigurationParam param = gson.fromJson(json, LspConfigurationParam.class);

		assertNotNull(param.getSettings());
		assertEquals(1, param.getSettings().size());
		assertNotNull(param.getSettings().get("endpoint"));
		assertEquals("https://api.snyk.io", param.getSettings().get("endpoint").getValue());
		assertNull(param.getFolderConfigs());
	}

	@Test
	void deserializesFolderOnly() {
		String json = """
				{
					"folderConfigs": [
						{
							"folderPath": "/home/user/project",
							"settings": {
								"base_branch": {
									"value": "main"
								}
							}
						}
					]
				}
				""";

		LspConfigurationParam param = gson.fromJson(json, LspConfigurationParam.class);

		assertNull(param.getSettings());
		assertNotNull(param.getFolderConfigs());
		assertEquals(1, param.getFolderConfigs().size());
		assertEquals("/home/user/project", param.getFolderConfigs().get(0).getFolderPath());
	}

	@Test
	void deserializesBothGlobalAndFolder() {
		String json = """
				{
					"settings": {
						"token": {
							"value": "secret",
							"isLocked": true
						}
					},
					"folderConfigs": [
						{
							"folderPath": "/project1",
							"settings": {
								"preferred_org": {
									"value": "org1"
								}
							}
						},
						{
							"folderPath": "/project2",
							"settings": {
								"preferred_org": {
									"value": "org2"
								}
							}
						}
					]
				}
				""";

		LspConfigurationParam param = gson.fromJson(json, LspConfigurationParam.class);

		assertNotNull(param.getSettings());
		assertEquals(1, param.getSettings().size());
		assertTrue(param.getSettings().get("token").getIsLocked());

		assertNotNull(param.getFolderConfigs());
		assertEquals(2, param.getFolderConfigs().size());
		assertEquals("/project1", param.getFolderConfigs().get(0).getFolderPath());
		assertEquals("/project2", param.getFolderConfigs().get(1).getFolderPath());
	}

	@Test
	void deserializesEmptyPayload() {
		String json = "{}";

		LspConfigurationParam param = gson.fromJson(json, LspConfigurationParam.class);

		assertNull(param.getSettings());
		assertNull(param.getFolderConfigs());
	}

	@Test
	void deserializesWithEmptyCollections() {
		String json = """
				{
					"settings": {},
					"folderConfigs": []
				}
				""";

		LspConfigurationParam param = gson.fromJson(json, LspConfigurationParam.class);

		assertNotNull(param.getSettings());
		assertTrue(param.getSettings().isEmpty());
		assertNotNull(param.getFolderConfigs());
		assertTrue(param.getFolderConfigs().isEmpty());
	}
}
