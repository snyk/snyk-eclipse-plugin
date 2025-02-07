package io.snyk.languageserver.protocolextension.messageObjects;

import java.util.Arrays;
import java.util.Objects;

public class PublishDiagnostics316Param {
	public PublishDiagnostics316Param() {

	}

	private String uri;
	private Diagnostic316[] diagnostics = new Diagnostic316[0];

	public String getUri() {
		return uri;
	}

	public void setUri(String uri) {
		this.uri = uri;
	}

	public Diagnostic316[] getDiagnostics() {
	    if (diagnostics == null) {
	        return new Diagnostic316[0];
	    }
	    return diagnostics.clone();
	}

	public void setDiagnostics(Diagnostic316... diagnostics) {
	    if (diagnostics != null) {
	        this.diagnostics = diagnostics.clone();
	    } else {
	        this.diagnostics = new Diagnostic316[0]; // or null, depending on your needs
	    }
	}
	
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + (diagnostics == null ? 0 : Arrays.hashCode(diagnostics));
		result = prime * result + Objects.hash(uri);
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		PublishDiagnostics316Param other = (PublishDiagnostics316Param) obj;
		return (diagnostics == null ? other.diagnostics == null : Arrays.equals(diagnostics, other.diagnostics))
				&& Objects.equals(uri, other.uri);
	}

	@Override
	public String toString() {
		return "PublishDiagnostic316Param [uri=" + uri + ", diagnostics="
				+ (diagnostics == null ? "null" : Arrays.toString(diagnostics)) + "]";
	}
}
