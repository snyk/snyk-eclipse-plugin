package io.snyk.eclipse.plugin.domain;

import java.util.List;

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

    public ScanResult() {
    }

    public List<Vuln> getVulnerabilities() {
        return this.vulnerabilities;
    }

    public Boolean getOk() {
        return this.ok;
    }

    public Integer getDependencyCount() {
        return this.dependencyCount;
    }

    public String getOrg() {
        return this.org;
    }

    public Boolean getIsPrivate() {
        return this.isPrivate;
    }

    public String getPackageManager() {
        return this.packageManager;
    }

    public String getSummary() {
        return this.summary;
    }

    public Integer getUniqueCount() {
        return this.uniqueCount;
    }

    public String getPath() {
        return this.path;
    }

    public void setVulnerabilities(List<Vuln> vulnerabilities) {
        this.vulnerabilities = vulnerabilities;
    }

    public void setOk(Boolean ok) {
        this.ok = ok;
    }

    public void setDependencyCount(Integer dependencyCount) {
        this.dependencyCount = dependencyCount;
    }

    public void setOrg(String org) {
        this.org = org;
    }

    public void setIsPrivate(Boolean isPrivate) {
        this.isPrivate = isPrivate;
    }

    public void setPackageManager(String packageManager) {
        this.packageManager = packageManager;
    }

    public void setSummary(String summary) {
        this.summary = summary;
    }

    public void setUniqueCount(Integer uniqueCount) {
        this.uniqueCount = uniqueCount;
    }

    public void setPath(String path) {
        this.path = path;
    }

    public boolean equals(final Object o) {
        if (o == this) return true;
        if (!(o instanceof ScanResult)) return false;
        final ScanResult other = (ScanResult) o;
        if (!other.canEqual(this)) return false;
        final Object this$vulnerabilities = this.getVulnerabilities();
        final Object other$vulnerabilities = other.getVulnerabilities();
        if (this$vulnerabilities == null ? other$vulnerabilities != null : !this$vulnerabilities.equals(other$vulnerabilities))
            return false;
        final Object this$ok = this.getOk();
        final Object other$ok = other.getOk();
        if (this$ok == null ? other$ok != null : !this$ok.equals(other$ok)) return false;
        final Object this$dependencyCount = this.getDependencyCount();
        final Object other$dependencyCount = other.getDependencyCount();
        if (this$dependencyCount == null ? other$dependencyCount != null : !this$dependencyCount.equals(other$dependencyCount))
            return false;
        final Object this$org = this.getOrg();
        final Object other$org = other.getOrg();
        if (this$org == null ? other$org != null : !this$org.equals(other$org)) return false;
        final Object this$isPrivate = this.getIsPrivate();
        final Object other$isPrivate = other.getIsPrivate();
        if (this$isPrivate == null ? other$isPrivate != null : !this$isPrivate.equals(other$isPrivate)) return false;
        final Object this$packageManager = this.getPackageManager();
        final Object other$packageManager = other.getPackageManager();
        if (this$packageManager == null ? other$packageManager != null : !this$packageManager.equals(other$packageManager))
            return false;
        final Object this$summary = this.getSummary();
        final Object other$summary = other.getSummary();
        if (this$summary == null ? other$summary != null : !this$summary.equals(other$summary)) return false;
        final Object this$uniqueCount = this.getUniqueCount();
        final Object other$uniqueCount = other.getUniqueCount();
        if (this$uniqueCount == null ? other$uniqueCount != null : !this$uniqueCount.equals(other$uniqueCount))
            return false;
        final Object this$path = this.getPath();
        final Object other$path = other.getPath();
        return this$path == null ? other$path == null : this$path.equals(other$path);
    }

    protected boolean canEqual(final Object other) {
        return other instanceof ScanResult;
    }

    public int hashCode() {
        final int PRIME = 59;
        int result = 1;
        final Object $vulnerabilities = this.getVulnerabilities();
        result = result * PRIME + ($vulnerabilities == null ? 43 : $vulnerabilities.hashCode());
        final Object $ok = this.getOk();
        result = result * PRIME + ($ok == null ? 43 : $ok.hashCode());
        final Object $dependencyCount = this.getDependencyCount();
        result = result * PRIME + ($dependencyCount == null ? 43 : $dependencyCount.hashCode());
        final Object $org = this.getOrg();
        result = result * PRIME + ($org == null ? 43 : $org.hashCode());
        final Object $isPrivate = this.getIsPrivate();
        result = result * PRIME + ($isPrivate == null ? 43 : $isPrivate.hashCode());
        final Object $packageManager = this.getPackageManager();
        result = result * PRIME + ($packageManager == null ? 43 : $packageManager.hashCode());
        final Object $summary = this.getSummary();
        result = result * PRIME + ($summary == null ? 43 : $summary.hashCode());
        final Object $uniqueCount = this.getUniqueCount();
        result = result * PRIME + ($uniqueCount == null ? 43 : $uniqueCount.hashCode());
        final Object $path = this.getPath();
        result = result * PRIME + ($path == null ? 43 : $path.hashCode());
        return result;
    }

    public String toString() {
        return "ScanResult(vulnerabilities=" + this.getVulnerabilities() + ", ok=" + this.getOk() + ", dependencyCount=" + this.getDependencyCount() + ", org=" + this.getOrg() + ", isPrivate=" + this.getIsPrivate() + ", packageManager=" + this.getPackageManager() + ", summary=" + this.getSummary() + ", uniqueCount=" + this.getUniqueCount() + ", path=" + this.getPath() + ")";
    }
}
