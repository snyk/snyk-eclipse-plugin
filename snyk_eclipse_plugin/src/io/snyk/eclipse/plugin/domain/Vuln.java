package io.snyk.eclipse.plugin.domain;

import java.util.List;
import java.util.stream.Collectors;

import lombok.Data;


@Data
public class Vuln {
    int cvssScore;
    String id;
    String language;
    String modificationTime;
    String moduleName;
    String packageManager;
    String packageName;
    List<Patch> patches;
    String publicationTime;
    String severity;
    String title;
    List<String> from;
    List<String> upgradePath;
    Boolean isUpgradable;
    Boolean isPatchable;
    String name;
    String version;
    
    public String getVulnTopLevelDependecy() {
    	if (from.size() >= 2) return from.get(1);
    	return from.get(0);
    }
    
    public String getFix() {
    	if (isUpgradable) {
    		if (upgradePath.size() > 1) return upgradePath.get(1);
        	return upgradePath.get(0);
    	}
    	
    	if (isPatchable) 
    		return patches.stream().map(Patch::getId).collect(Collectors.joining(", "));
    	    	
    	return null;    	
    }
    
    public String getUrl() {
    	if (id == null) return null;
    	return "https://snyk.io/vuln/" + id;
    }
    
    
    public String printFrom() {
    	return from.stream().collect(Collectors.joining(" -> "));
    }
    
    public String printUpgrade() {
    	return upgradePath.stream().collect(Collectors.joining(" -> "));
    }
}
