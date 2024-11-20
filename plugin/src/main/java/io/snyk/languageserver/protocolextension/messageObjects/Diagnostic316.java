package io.snyk.languageserver.protocolextension.messageObjects;

import java.util.Objects;

import io.snyk.languageserver.protocolextension.messageObjects.scanResults.Issue;

public class Diagnostic316 {
	public Diagnostic316() {
		
	}

	private Issue data;
	private String source;
	
    public String getSource() {
		return source;
	}

	public void setSource(String source) {
		this.source = source;
	}


    public Issue getData() {
        return data;
    }
    
    public void setData(Issue data) {
        this.data = data;
    }
    
    @Override
	public int hashCode() {
		return Objects.hash(data, source);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		Diagnostic316 other = (Diagnostic316) obj;
		return Objects.equals(data, other.data) && Objects.equals(source, other.source);
	}

	@Override
	public String toString() {
		return "Diagnostic316 [data=" + data + ", source=" + source + "]";
	}    
}