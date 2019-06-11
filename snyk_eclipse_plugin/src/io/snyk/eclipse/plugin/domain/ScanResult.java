package io.snyk.eclipse.plugin.domain;

import lombok.Data;

import java.util.List;

@Data
public class ScanResult {

     List<Vuln> vulnerabilities;
     Boolean ok;
     Integer dependencyCount;
     String org;
     String policyes;
     Boolean isPrivate;
     String licensesPolicy;
     String packageManager;
     String ignoreSettings;
     String summary;
     String filesystemPolicy;

     Integer uniqueCount;
     String path;


}
