package io.snyk.eclipse.plugin.views;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IProject;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@AllArgsConstructor
@Builder
public class DisplayModel {
	
	public DisplayModel parent;
	public List<DisplayModel> children = new ArrayList<>();
	
	public String id;
	public String description;
	public String severity;
	public String dependecy;
	public String vulnPackage;
	public String fix;
	public String vulnPath;
	
	public String projectName;
	public String fileName;
	public String link;
	
	public IProject iProject;

}
