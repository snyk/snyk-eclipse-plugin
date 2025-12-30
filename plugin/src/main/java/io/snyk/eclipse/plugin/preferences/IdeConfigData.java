// ABOUTME: Main configuration data class for IDE settings
// ABOUTME: Encompasses all Snyk plugin settings including scan settings, authentication, CLI
// configuration, and filters
package io.snyk.eclipse.plugin.preferences;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import java.util.List;
import java.util.Map;

@JsonIgnoreProperties(ignoreUnknown = true)
public class IdeConfigData {

  // Inner data classes

  @JsonIgnoreProperties(ignoreUnknown = true)
  public static class IssueViewOptions {
    private Boolean openIssues;
    private Boolean ignoredIssues;

    public Boolean getOpenIssues() {
      return openIssues;
    }

    public void setOpenIssues(Boolean openIssues) {
      this.openIssues = openIssues;
    }

    public Boolean getIgnoredIssues() {
      return ignoredIssues;
    }

    public void setIgnoredIssues(Boolean ignoredIssues) {
      this.ignoredIssues = ignoredIssues;
    }
  }

  @JsonIgnoreProperties(ignoreUnknown = true)
  public static class FilterSeverity {
    private Boolean critical;
    private Boolean high;
    private Boolean medium;
    private Boolean low;

    public Boolean getCritical() {
      return critical;
    }

    public void setCritical(Boolean critical) {
      this.critical = critical;
    }

    public Boolean getHigh() {
      return high;
    }

    public void setHigh(Boolean high) {
      this.high = high;
    }

    public Boolean getMedium() {
      return medium;
    }

    public void setMedium(Boolean medium) {
      this.medium = medium;
    }

    public Boolean getLow() {
      return low;
    }

    public void setLow(Boolean low) {
      this.low = low;
    }
  }

  @JsonIgnoreProperties(ignoreUnknown = true)
  public static class ScanCommandConfigData {
    private String preScanCommand;
    private Boolean preScanOnlyReferenceFolder;
    private String postScanCommand;
    private Boolean postScanOnlyReferenceFolder;

    public String getPreScanCommand() {
      return preScanCommand;
    }

    public void setPreScanCommand(String preScanCommand) {
      this.preScanCommand = preScanCommand;
    }

    public Boolean getPreScanOnlyReferenceFolder() {
      return preScanOnlyReferenceFolder;
    }

    public void setPreScanOnlyReferenceFolder(Boolean preScanOnlyReferenceFolder) {
      this.preScanOnlyReferenceFolder = preScanOnlyReferenceFolder;
    }

    public String getPostScanCommand() {
      return postScanCommand;
    }

    public void setPostScanCommand(String postScanCommand) {
      this.postScanCommand = postScanCommand;
    }

    public Boolean getPostScanOnlyReferenceFolder() {
      return postScanOnlyReferenceFolder;
    }

    public void setPostScanOnlyReferenceFolder(Boolean postScanOnlyReferenceFolder) {
      this.postScanOnlyReferenceFolder = postScanOnlyReferenceFolder;
    }
  }

  @JsonIgnoreProperties(ignoreUnknown = true)
  public static class FolderConfigData {
    private String folderPath;
    private String additionalParameters;
    private String additionalEnv;
    private String preferredOrg;
    private String autoDeterminedOrg;
    private Boolean orgSetByUser;
    private Map<String, ScanCommandConfigData> scanCommandConfig;

    public String getFolderPath() {
      return folderPath;
    }

    public void setFolderPath(String folderPath) {
      this.folderPath = folderPath;
    }

    public String getAdditionalParameters() {
      return additionalParameters;
    }

    public void setAdditionalParameters(String additionalParameters) {
      this.additionalParameters = additionalParameters;
    }

    public String getAdditionalEnv() {
      return additionalEnv;
    }

    public void setAdditionalEnv(String additionalEnv) {
      this.additionalEnv = additionalEnv;
    }

    public String getPreferredOrg() {
      return preferredOrg;
    }

    public void setPreferredOrg(String preferredOrg) {
      this.preferredOrg = preferredOrg;
    }

    public String getAutoDeterminedOrg() {
      return autoDeterminedOrg;
    }

    public void setAutoDeterminedOrg(String autoDeterminedOrg) {
      this.autoDeterminedOrg = autoDeterminedOrg;
    }

    public Boolean getOrgSetByUser() {
      return orgSetByUser;
    }

    public void setOrgSetByUser(Boolean orgSetByUser) {
      this.orgSetByUser = orgSetByUser;
    }

    public Map<String, ScanCommandConfigData> getScanCommandConfig() {
      return scanCommandConfig;
    }

    public void setScanCommandConfig(Map<String, ScanCommandConfigData> scanCommandConfig) {
      this.scanCommandConfig = scanCommandConfig;
    }
  }

  // Main class fields

  // Scan Settings
  private Boolean activateSnykOpenSource;
  private Boolean activateSnykCode;
  private Boolean activateSnykIac;
  private String scanningMode;

  // Issue View Settings
  private IssueViewOptions issueViewOptions;
  private Boolean enableDeltaFindings;

  // Authentication Settings
  private String authenticationMethod;

  // Connection Settings
  private String endpoint;
  private String token;
  private Boolean insecure;
  private String organization;

  // Trusted Folders
  private List<String> trustedFolders;

  // CLI Settings
  private String cliPath;
  private Boolean manageBinariesAutomatically;
  private String cliBaseDownloadURL;
  private String cliReleaseChannel;

  // Filter Settings
  private FilterSeverity filterSeverity;
  private Integer riskScoreThreshold;

  // Folder Configs
  private List<FolderConfigData> folderConfigs;

  // Form Type Indicator
  private Boolean isFallbackForm;

  // Getters and Setters

  public Boolean getActivateSnykOpenSource() {
    return activateSnykOpenSource;
  }

  public void setActivateSnykOpenSource(Boolean activateSnykOpenSource) {
    this.activateSnykOpenSource = activateSnykOpenSource;
  }

  public Boolean getActivateSnykCode() {
    return activateSnykCode;
  }

  public void setActivateSnykCode(Boolean activateSnykCode) {
    this.activateSnykCode = activateSnykCode;
  }

  public Boolean getActivateSnykIac() {
    return activateSnykIac;
  }

  public void setActivateSnykIac(Boolean activateSnykIac) {
    this.activateSnykIac = activateSnykIac;
  }

  public String getScanningMode() {
    return scanningMode;
  }

  public void setScanningMode(String scanningMode) {
    this.scanningMode = scanningMode;
  }

  public IssueViewOptions getIssueViewOptions() {
    return issueViewOptions;
  }

  public void setIssueViewOptions(IssueViewOptions issueViewOptions) {
    this.issueViewOptions = issueViewOptions;
  }

  public Boolean getEnableDeltaFindings() {
    return enableDeltaFindings;
  }

  public void setEnableDeltaFindings(Boolean enableDeltaFindings) {
    this.enableDeltaFindings = enableDeltaFindings;
  }

  public String getAuthenticationMethod() {
    return authenticationMethod;
  }

  public void setAuthenticationMethod(String authenticationMethod) {
    this.authenticationMethod = authenticationMethod;
  }

  public String getEndpoint() {
    return endpoint;
  }

  public void setEndpoint(String endpoint) {
    this.endpoint = endpoint;
  }

  public String getToken() {
    return token;
  }

  public void setToken(String token) {
    this.token = token;
  }

  public Boolean getInsecure() {
    return insecure;
  }

  public void setInsecure(Boolean insecure) {
    this.insecure = insecure;
  }

  public String getOrganization() {
    return organization;
  }

  public void setOrganization(String organization) {
    this.organization = organization;
  }

  public List<String> getTrustedFolders() {
    return trustedFolders;
  }

  public void setTrustedFolders(List<String> trustedFolders) {
    this.trustedFolders = trustedFolders;
  }

  public String getCliPath() {
    return cliPath;
  }

  public void setCliPath(String cliPath) {
    this.cliPath = cliPath;
  }

  public Boolean getManageBinariesAutomatically() {
    return manageBinariesAutomatically;
  }

  public void setManageBinariesAutomatically(Boolean manageBinariesAutomatically) {
    this.manageBinariesAutomatically = manageBinariesAutomatically;
  }

  public String getCliBaseDownloadURL() {
    return cliBaseDownloadURL;
  }

  public void setCliBaseDownloadURL(String cliBaseDownloadURL) {
    this.cliBaseDownloadURL = cliBaseDownloadURL;
  }

  public String getCliReleaseChannel() {
    return cliReleaseChannel;
  }

  public void setCliReleaseChannel(String cliReleaseChannel) {
    this.cliReleaseChannel = cliReleaseChannel;
  }

  public FilterSeverity getFilterSeverity() {
    return filterSeverity;
  }

  public void setFilterSeverity(FilterSeverity filterSeverity) {
    this.filterSeverity = filterSeverity;
  }

  public Integer getRiskScoreThreshold() {
    return riskScoreThreshold;
  }

  public void setRiskScoreThreshold(Integer riskScoreThreshold) {
    this.riskScoreThreshold = riskScoreThreshold;
  }

  public List<FolderConfigData> getFolderConfigs() {
    return folderConfigs;
  }

  public void setFolderConfigs(List<FolderConfigData> folderConfigs) {
    this.folderConfigs = folderConfigs;
  }

  public Boolean getIsFallbackForm() {
    return isFallbackForm;
  }

  public void setIsFallbackForm(Boolean isFallbackForm) {
    this.isFallbackForm = isFallbackForm;
  }
}
