package io.snyk.eclipse.plugin.views;

import org.eclipse.core.resources.IProject;

import java.util.ArrayList;
import java.util.List;

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

  public DisplayModel(DisplayModel parent, List<DisplayModel> children, String id, String description, String severity, String dependecy, String vulnPackage, String fix, String vulnPath, String projectName, String fileName, String link, IProject iProject) {
    this.parent = parent;
    this.children = children;
    this.id = id;
    this.description = description;
    this.severity = severity;
    this.dependecy = dependecy;
    this.vulnPackage = vulnPackage;
    this.fix = fix;
    this.vulnPath = vulnPath;
    this.projectName = projectName;
    this.fileName = fileName;
    this.link = link;
    this.iProject = iProject;
  }

  public DisplayModel() {
  }

  public static DisplayModelBuilder builder() {
    return new DisplayModelBuilder();
  }

  public static class DisplayModelBuilder {
    private DisplayModel parent;
    private List<DisplayModel> children;
    private String id;
    private String description;
    private String severity;
    private String dependecy;
    private String vulnPackage;
    private String fix;
    private String vulnPath;
    private String projectName;
    private String fileName;
    private String link;
    private IProject iProject;

    DisplayModelBuilder() {
    }

    public DisplayModelBuilder parent(DisplayModel parent) {
      this.parent = parent;
      return this;
    }

    public DisplayModelBuilder children(List<DisplayModel> children) {
      this.children = children;
      return this;
    }

    public DisplayModelBuilder id(String id) {
      this.id = id;
      return this;
    }

    public DisplayModelBuilder description(String description) {
      this.description = description;
      return this;
    }

    public DisplayModelBuilder severity(String severity) {
      this.severity = severity;
      return this;
    }

    public DisplayModelBuilder dependecy(String dependecy) {
      this.dependecy = dependecy;
      return this;
    }

    public DisplayModelBuilder vulnPackage(String vulnPackage) {
      this.vulnPackage = vulnPackage;
      return this;
    }

    public DisplayModelBuilder fix(String fix) {
      this.fix = fix;
      return this;
    }

    public DisplayModelBuilder vulnPath(String vulnPath) {
      this.vulnPath = vulnPath;
      return this;
    }

    public DisplayModelBuilder projectName(String projectName) {
      this.projectName = projectName;
      return this;
    }

    public DisplayModelBuilder fileName(String fileName) {
      this.fileName = fileName;
      return this;
    }

    public DisplayModelBuilder link(String link) {
      this.link = link;
      return this;
    }

    public DisplayModelBuilder iProject(IProject iProject) {
      this.iProject = iProject;
      return this;
    }

    public DisplayModel build() {
      return new DisplayModel(parent, children, id, description, severity, dependecy, vulnPackage, fix, vulnPath, projectName, fileName, link, iProject);
    }

    public String toString() {
      return "DisplayModel.DisplayModelBuilder(parent=" + this.parent + ", children=" + this.children + ", id=" + this.id + ", description=" + this.description + ", severity=" + this.severity + ", dependecy=" + this.dependecy + ", vulnPackage=" + this.vulnPackage + ", fix=" + this.fix + ", vulnPath=" + this.vulnPath + ", projectName=" + this.projectName + ", fileName=" + this.fileName + ", link=" + this.link + ", iProject=" + this.iProject + ")";
    }
  }
}
