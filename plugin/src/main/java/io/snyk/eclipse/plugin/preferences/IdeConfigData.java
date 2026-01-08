// ABOUTME: Main configuration data class for IDE settings
// ABOUTME: Encompasses all Snyk plugin settings including scan settings, authentication, CLI
// configuration, and filters
package io.snyk.eclipse.plugin.preferences;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import java.util.List;
import java.util.Map;

@JsonIgnoreProperties(ignoreUnknown = true)
public record IdeConfigData(
    // Scan Settings
    Boolean activateSnykOpenSource,
    Boolean activateSnykCode,
    Boolean activateSnykIac,
    String scanningMode,

    // Issue View Settings
    IssueViewOptions issueViewOptions,
    Boolean enableDeltaFindings,

    // Authentication Settings
    String authenticationMethod,

    // Connection Settings
    String endpoint,
    String token,
    Boolean insecure,
    String organization,

    // Trusted Folders
    List<String> trustedFolders,

    // CLI Settings
    String cliPath,
    Boolean manageBinariesAutomatically,
    String cliBaseDownloadURL,
    String cliReleaseChannel,

    // Filter Settings
    FilterSeverity filterSeverity,
    Integer riskScoreThreshold,

    // Folder Configs
    List<FolderConfigData> folderConfigs,

    // Form Type Indicator
    Boolean isFallbackForm
) {

  // Inner data classes

  @JsonIgnoreProperties(ignoreUnknown = true)
  public record IssueViewOptions(
      Boolean openIssues,
      Boolean ignoredIssues
  ) {}

  @JsonIgnoreProperties(ignoreUnknown = true)
  public record FilterSeverity(
      Boolean critical,
      Boolean high,
      Boolean medium,
      Boolean low
  ) {}

  @JsonIgnoreProperties(ignoreUnknown = true)
  public record ScanCommandConfigData(
      String preScanCommand,
      Boolean preScanOnlyReferenceFolder,
      String postScanCommand,
      Boolean postScanOnlyReferenceFolder
  ) {}

  @JsonIgnoreProperties(ignoreUnknown = true)
  public record FolderConfigData(
      String folderPath,
      List<String> additionalParameters,
      String additionalEnv,
      String preferredOrg,
      String autoDeterminedOrg,
      Boolean orgSetByUser,
      Map<String, ScanCommandConfigData> scanCommandConfig
  ) {}
}
