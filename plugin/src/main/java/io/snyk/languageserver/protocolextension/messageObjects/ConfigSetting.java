package io.snyk.languageserver.protocolextension.messageObjects;

import com.google.gson.annotations.SerializedName;

public class ConfigSetting {

	private Object value;

	private Boolean changed;

	private String source;

	@SerializedName("origin_scope")
	private String originScope;

	@SerializedName("is_locked")
	private Boolean isLocked;

	public Object getValue() {
		return value;
	}

	public Boolean getChanged() {
		return changed;
	}

	public String getSource() {
		return source;
	}

	public String getOriginScope() {
		return originScope;
	}

	public Boolean getIsLocked() {
		return isLocked;
	}

	public static ConfigSetting outbound(Object value, boolean changed) {
		ConfigSetting setting = new ConfigSetting();
		setting.value = value;
		setting.changed = changed;
		return setting;
	}

	void setValue(Object value) {
		this.value = value;
	}

	void setChanged(Boolean changed) {
		this.changed = changed;
	}

	void setSource(String source) {
		this.source = source;
	}

	void setOriginScope(String originScope) {
		this.originScope = originScope;
	}

	void setIsLocked(Boolean isLocked) {
		this.isLocked = isLocked;
	}
}
