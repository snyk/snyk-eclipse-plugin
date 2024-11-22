package io.snyk.languageserver.protocolextension.messageObjects;

import java.util.Objects;

public class FeatureFlagStatus {
	private Boolean ok;
    private String userMessage;
    
	public Boolean getOk() {
		return ok;
	}
	public void setOk(Boolean ok) {
		this.ok = ok;
	}
	public String getUserMessage() {
		return userMessage;
	}
	public void setUserMessage(String userMessage) {
		this.userMessage = userMessage;
	}
	
    @Override
	public String toString() {
		return "FeatureFlagStatus [ok=" + ok + ", userMessage=" + userMessage + "]";
	}
	@Override
	public int hashCode() {
		return Objects.hash(ok, userMessage);
	}
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		FeatureFlagStatus other = (FeatureFlagStatus) obj;
		return Objects.equals(ok, other.ok) && Objects.equals(userMessage, other.userMessage);
	}
}
