package io.snyk.languageserver.download;

import java.time.Instant;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

/**
 * Json data class for Metadata created from build
 * 
 * @author bdoetsch
 *
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class LsMetadata {
	private String tag;
	private String version;
	private String commit;

	public String getTag() {
		return tag;
	}

	public void setTag(String tag) {
		this.tag = tag;
	}

	public String getVersion() {
		return version;
	}

	public void setVersion(String version) {
		this.version = version;
	}

	public String getCommit() {
		return commit;
	}

	public void setCommit(String commit) {
		this.commit = commit;
	}
}
