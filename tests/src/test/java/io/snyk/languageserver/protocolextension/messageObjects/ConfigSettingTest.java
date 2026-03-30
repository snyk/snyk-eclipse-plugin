package io.snyk.languageserver.protocolextension.messageObjects;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import com.google.gson.Gson;

class ConfigSettingTest {

	private final Gson gson = new Gson();

	@Test
	void deserializesAllFields() {
		String json = """
				{
					"value": "https://api.snyk.io",
					"changed": true,
					"source": "cli",
					"origin_scope": "machine",
					"is_locked": false
				}
				""";

		ConfigSetting setting = gson.fromJson(json, ConfigSetting.class);

		assertEquals("https://api.snyk.io", setting.getValue());
		assertTrue(setting.getChanged());
		assertEquals("cli", setting.getSource());
		assertEquals("machine", setting.getOriginScope());
		assertFalse(setting.getIsLocked());
	}

	@Test
	void deserializesWithNullFields() {
		String json = """
				{
					"value": "test"
				}
				""";

		ConfigSetting setting = gson.fromJson(json, ConfigSetting.class);

		assertEquals("test", setting.getValue());
		assertNull(setting.getChanged());
		assertNull(setting.getSource());
		assertNull(setting.getOriginScope());
		assertNull(setting.getIsLocked());
	}

	@Test
	void deserializesWithBooleanValue() {
		String json = """
				{
					"value": true,
					"changed": false,
					"source": "default",
					"origin_scope": "global",
					"is_locked": true
				}
				""";

		ConfigSetting setting = gson.fromJson(json, ConfigSetting.class);

		assertEquals(true, setting.getValue());
		assertFalse(setting.getChanged());
		assertEquals("default", setting.getSource());
		assertEquals("global", setting.getOriginScope());
		assertTrue(setting.getIsLocked());
	}

	@Test
	void deserializesWithNumericValue() {
		String json = """
				{
					"value": 42
				}
				""";

		ConfigSetting setting = gson.fromJson(json, ConfigSetting.class);

		assertEquals(42.0, setting.getValue());
	}

	@Test
	void deserializesWithNullValue() {
		String json = """
				{
					"value": null,
					"changed": null,
					"is_locked": null
				}
				""";

		ConfigSetting setting = gson.fromJson(json, ConfigSetting.class);

		assertNull(setting.getValue());
		assertNull(setting.getChanged());
		assertNull(setting.getIsLocked());
	}

	@Test
	void deserializesWithListValue() {
		String json = """
				{
					"value": ["a", "b", "c"]
				}
				""";

		ConfigSetting setting = gson.fromJson(json, ConfigSetting.class);

		assertTrue(setting.getValue() instanceof java.util.List);
	}

	@Test
	void deserializesWithMapValue() {
		String json = """
				{
					"value": {"key": "val"}
				}
				""";

		ConfigSetting setting = gson.fromJson(json, ConfigSetting.class);

		assertTrue(setting.getValue() instanceof java.util.Map);
	}

	@Test
	void outboundFactoryCreatesSettingWithValueAndChanged() {
		ConfigSetting setting = ConfigSetting.outbound("endpoint-value", true);

		assertEquals("endpoint-value", setting.getValue());
		assertTrue(setting.getChanged());
		assertNull(setting.getSource());
		assertNull(setting.getOriginScope());
		assertNull(setting.getIsLocked());
	}

	@Test
	void outboundFactorySupportsNullValueForReset() {
		ConfigSetting setting = ConfigSetting.outbound(null, true);

		assertNull(setting.getValue());
		assertTrue(setting.getChanged());
	}
}
