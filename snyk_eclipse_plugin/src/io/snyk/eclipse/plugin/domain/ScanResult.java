package io.snyk.eclipse.plugin.domain;

import lombok.Data;

import java.util.List;

@Data
public class ScanResult {

     List<Vuln> vulnerabilities;
     Boolean ok;
     Integer dependencyCount;
     String org;
     Boolean isPrivate;
     String packageManager;
     String summary;
     Integer uniqueCount;
     String path;
}
