package io.snyk.eclipse.plugin.domain;

import java.util.List;

import lombok.Data;

@Data
public class Patch {
    List<String> comments;
    String id;
    String modificationTime;
    List<String> urls;
    String version;

}
