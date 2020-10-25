package io.snyk.eclipse.plugin.domain;

import com.fasterxml.jackson.annotation.JsonProperty;

import lombok.Data;

@Data
public class LatestReleaseInfo {
	Long id;
    String url;
    String name;
    
    @JsonProperty("tag_name")
    String tagName;
}
