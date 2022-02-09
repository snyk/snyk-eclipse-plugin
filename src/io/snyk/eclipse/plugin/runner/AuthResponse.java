package io.snyk.eclipse.plugin.runner;

public class AuthResponse {
	boolean ok;
	String api;

	public AuthResponse() {
	}

	public boolean isOk() {
		return this.ok;
	}

	public String getApi() {
		return this.api;
	}

	public void setOk(boolean ok) {
		this.ok = ok;
	}

	public void setApi(String api) {
		this.api = api;
	}

	public boolean equals(final Object o) {
		if (o == this) return true;
		if (!(o instanceof AuthResponse)) return false;
		final AuthResponse other = (AuthResponse) o;
		if (!other.canEqual((Object) this)) return false;
		if (this.isOk() != other.isOk()) return false;
		final Object this$api = this.getApi();
		final Object other$api = other.getApi();
		if (this$api == null ? other$api != null : !this$api.equals(other$api)) return false;
		return true;
	}

	protected boolean canEqual(final Object other) {
		return other instanceof AuthResponse;
	}

	public int hashCode() {
		final int PRIME = 59;
		int result = 1;
		result = result * PRIME + (this.isOk() ? 79 : 97);
		final Object $api = this.getApi();
		result = result * PRIME + ($api == null ? 43 : $api.hashCode());
		return result;
	}

	public String toString() {
		return "AuthResponse(ok=" + this.isOk() + ", api=" + this.getApi() + ")";
	}
}
