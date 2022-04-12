package io.snyk.eclipse.plugin.domain;

import java.util.List;
import java.util.stream.Collectors;


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

  public Vuln() {
  }

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
    return String.join(" -> ", from);
  }

  public String printUpgrade() {
    return String.join(" -> ", upgradePath);
  }

  public int getCvssScore() {
    return this.cvssScore;
  }

  public String getId() {
    return this.id;
  }

  public String getLanguage() {
    return this.language;
  }

  public String getModificationTime() {
    return this.modificationTime;
  }

  public String getModuleName() {
    return this.moduleName;
  }

  public String getPackageManager() {
    return this.packageManager;
  }

  public String getPackageName() {
    return this.packageName;
  }

  public List<Patch> getPatches() {
    return this.patches;
  }

  public String getPublicationTime() {
    return this.publicationTime;
  }

  public String getSeverity() {
    return this.severity;
  }

  public String getTitle() {
    return this.title;
  }

  public List<String> getFrom() {
    return this.from;
  }

  public List<String> getUpgradePath() {
    return this.upgradePath;
  }

  public Boolean getIsUpgradable() {
    return this.isUpgradable;
  }

  public Boolean getIsPatchable() {
    return this.isPatchable;
  }

  public String getName() {
    return this.name;
  }

  public String getVersion() {
    return this.version;
  }

  public void setCvssScore(int cvssScore) {
    this.cvssScore = cvssScore;
  }

  public void setId(String id) {
    this.id = id;
  }

  public void setLanguage(String language) {
    this.language = language;
  }

  public void setModificationTime(String modificationTime) {
    this.modificationTime = modificationTime;
  }

  public void setModuleName(String moduleName) {
    this.moduleName = moduleName;
  }

  public void setPackageManager(String packageManager) {
    this.packageManager = packageManager;
  }

  public void setPackageName(String packageName) {
    this.packageName = packageName;
  }

  public void setPatches(List<Patch> patches) {
    this.patches = patches;
  }

  public void setPublicationTime(String publicationTime) {
    this.publicationTime = publicationTime;
  }

  public void setSeverity(String severity) {
    this.severity = severity;
  }

  public void setTitle(String title) {
    this.title = title;
  }

  public void setFrom(List<String> from) {
    this.from = from;
  }

  public void setUpgradePath(List<String> upgradePath) {
    this.upgradePath = upgradePath;
  }

  public void setIsUpgradable(Boolean isUpgradable) {
    this.isUpgradable = isUpgradable;
  }

  public void setIsPatchable(Boolean isPatchable) {
    this.isPatchable = isPatchable;
  }

  public void setName(String name) {
    this.name = name;
  }

  public void setVersion(String version) {
    this.version = version;
  }

  public boolean equals(final Object o) {
    if (o == this) return true;
    if (!(o instanceof Vuln)) return false;
    final Vuln other = (Vuln) o;
    if (!other.canEqual(this)) return false;
    if (this.getCvssScore() != other.getCvssScore()) return false;
    final Object this$id = this.getId();
    final Object other$id = other.getId();
    if (this$id == null ? other$id != null : !this$id.equals(other$id)) return false;
    final Object this$language = this.getLanguage();
    final Object other$language = other.getLanguage();
    if (this$language == null ? other$language != null : !this$language.equals(other$language)) return false;
    final Object this$modificationTime = this.getModificationTime();
    final Object other$modificationTime = other.getModificationTime();
    if (this$modificationTime == null ? other$modificationTime != null : !this$modificationTime.equals(other$modificationTime))
      return false;
    final Object this$moduleName = this.getModuleName();
    final Object other$moduleName = other.getModuleName();
    if (this$moduleName == null ? other$moduleName != null : !this$moduleName.equals(other$moduleName))
      return false;
    final Object this$packageManager = this.getPackageManager();
    final Object other$packageManager = other.getPackageManager();
    if (this$packageManager == null ? other$packageManager != null : !this$packageManager.equals(other$packageManager))
      return false;
    final Object this$packageName = this.getPackageName();
    final Object other$packageName = other.getPackageName();
    if (this$packageName == null ? other$packageName != null : !this$packageName.equals(other$packageName))
      return false;
    final Object this$patches = this.getPatches();
    final Object other$patches = other.getPatches();
    if (this$patches == null ? other$patches != null : !this$patches.equals(other$patches)) return false;
    final Object this$publicationTime = this.getPublicationTime();
    final Object other$publicationTime = other.getPublicationTime();
    if (this$publicationTime == null ? other$publicationTime != null : !this$publicationTime.equals(other$publicationTime))
      return false;
    final Object this$severity = this.getSeverity();
    final Object other$severity = other.getSeverity();
    if (this$severity == null ? other$severity != null : !this$severity.equals(other$severity)) return false;
    final Object this$title = this.getTitle();
    final Object other$title = other.getTitle();
    if (this$title == null ? other$title != null : !this$title.equals(other$title)) return false;
    final Object this$from = this.getFrom();
    final Object other$from = other.getFrom();
    if (this$from == null ? other$from != null : !this$from.equals(other$from)) return false;
    final Object this$upgradePath = this.getUpgradePath();
    final Object other$upgradePath = other.getUpgradePath();
    if (this$upgradePath == null ? other$upgradePath != null : !this$upgradePath.equals(other$upgradePath))
      return false;
    final Object this$isUpgradable = this.getIsUpgradable();
    final Object other$isUpgradable = other.getIsUpgradable();
    if (this$isUpgradable == null ? other$isUpgradable != null : !this$isUpgradable.equals(other$isUpgradable))
      return false;
    final Object this$isPatchable = this.getIsPatchable();
    final Object other$isPatchable = other.getIsPatchable();
    if (this$isPatchable == null ? other$isPatchable != null : !this$isPatchable.equals(other$isPatchable))
      return false;
    final Object this$name = this.getName();
    final Object other$name = other.getName();
    if (this$name == null ? other$name != null : !this$name.equals(other$name)) return false;
    final Object this$version = this.getVersion();
    final Object other$version = other.getVersion();
    return this$version == null ? other$version == null : this$version.equals(other$version);
  }

  protected boolean canEqual(final Object other) {
    return other instanceof Vuln;
  }

  public int hashCode() {
    final int PRIME = 59;
    int result = 1;
    result = result * PRIME + this.getCvssScore();
    final Object $id = this.getId();
    result = result * PRIME + ($id == null ? 43 : $id.hashCode());
    final Object $language = this.getLanguage();
    result = result * PRIME + ($language == null ? 43 : $language.hashCode());
    final Object $modificationTime = this.getModificationTime();
    result = result * PRIME + ($modificationTime == null ? 43 : $modificationTime.hashCode());
    final Object $moduleName = this.getModuleName();
    result = result * PRIME + ($moduleName == null ? 43 : $moduleName.hashCode());
    final Object $packageManager = this.getPackageManager();
    result = result * PRIME + ($packageManager == null ? 43 : $packageManager.hashCode());
    final Object $packageName = this.getPackageName();
    result = result * PRIME + ($packageName == null ? 43 : $packageName.hashCode());
    final Object $patches = this.getPatches();
    result = result * PRIME + ($patches == null ? 43 : $patches.hashCode());
    final Object $publicationTime = this.getPublicationTime();
    result = result * PRIME + ($publicationTime == null ? 43 : $publicationTime.hashCode());
    final Object $severity = this.getSeverity();
    result = result * PRIME + ($severity == null ? 43 : $severity.hashCode());
    final Object $title = this.getTitle();
    result = result * PRIME + ($title == null ? 43 : $title.hashCode());
    final Object $from = this.getFrom();
    result = result * PRIME + ($from == null ? 43 : $from.hashCode());
    final Object $upgradePath = this.getUpgradePath();
    result = result * PRIME + ($upgradePath == null ? 43 : $upgradePath.hashCode());
    final Object $isUpgradable = this.getIsUpgradable();
    result = result * PRIME + ($isUpgradable == null ? 43 : $isUpgradable.hashCode());
    final Object $isPatchable = this.getIsPatchable();
    result = result * PRIME + ($isPatchable == null ? 43 : $isPatchable.hashCode());
    final Object $name = this.getName();
    result = result * PRIME + ($name == null ? 43 : $name.hashCode());
    final Object $version = this.getVersion();
    result = result * PRIME + ($version == null ? 43 : $version.hashCode());
    return result;
  }

  public String toString() {
    return "Vuln(cvssScore=" + this.getCvssScore() + ", id=" + this.getId() + ", language=" + this.getLanguage() + ", modificationTime=" + this.getModificationTime() + ", moduleName=" + this.getModuleName() + ", packageManager=" + this.getPackageManager() + ", packageName=" + this.getPackageName() + ", patches=" + this.getPatches() + ", publicationTime=" + this.getPublicationTime() + ", severity=" + this.getSeverity() + ", title=" + this.getTitle() + ", from=" + this.getFrom() + ", upgradePath=" + this.getUpgradePath() + ", isUpgradable=" + this.getIsUpgradable() + ", isPatchable=" + this.getIsPatchable() + ", name=" + this.getName() + ", version=" + this.getVersion() + ")";
  }
}
