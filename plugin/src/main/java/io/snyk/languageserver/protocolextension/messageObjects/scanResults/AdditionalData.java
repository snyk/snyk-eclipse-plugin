package io.snyk.languageserver.protocolextension.messageObjects.scanResults;

public class AdditionalData {
	public AdditionalData() {
		
	}
	
    // Code
    private String message;
	private String[] cwe;
    private String text;
    private boolean isSecurityType;
    private boolean hasAIFix;

    // OSS + Code
    private String ruleId;
    private String details;

    // OSS
    private String license;
    private String description;
    private String language;
    private String packageManager;
    private String packageName;
    private String name;
    private String version;
    private String exploit;
    private String projectName;
    private String displayTargetFile;

    // IaC
    private String publicId;
    private String customUIContent;
    
    public String getMessage() {
		return message;
	}
	public void setMessage(String message) {
		this.message = message;
	}
	public String[] getCwe() {
		return cwe;
	}
	public void setCwe(String[] cwe) {
		this.cwe = cwe;
	}
	public String getText() {
		return text;
	}
	public void setText(String text) {
		this.text = text;
	}
	public boolean isSecurityType() {
		return isSecurityType;
	}
	public void setSecurityType(boolean isSecurityType) {
		this.isSecurityType = isSecurityType;
	}
	public boolean isHasAIFix() {
		return hasAIFix;
	}
	public void setHasAIFix(boolean hasAIFix) {
		this.hasAIFix = hasAIFix;
	}
	public String getRuleId() {
		return ruleId;
	}
	public void setRuleId(String ruleId) {
		this.ruleId = ruleId;
	}
	public String getDetails() {
		return details;
	}
	public void setDetails(String details) {
		this.details = details;
	}
	public String getLicense() {
		return license;
	}
	public void setLicense(String license) {
		this.license = license;
	}
	public String getDescription() {
		return description;
	}
	public void setDescription(String description) {
		this.description = description;
	}
	public String getLanguage() {
		return language;
	}
	public void setLanguage(String language) {
		this.language = language;
	}
	public String getPackageManager() {
		return packageManager;
	}
	public void setPackageManager(String packageManager) {
		this.packageManager = packageManager;
	}
	public String getPackageName() {
		return packageName;
	}
	public void setPackageName(String packageName) {
		this.packageName = packageName;
	}
	public String getName() {
		return name;
	}
	public void setName(String name) {
		this.name = name;
	}
	public String getVersion() {
		return version;
	}
	public void setVersion(String version) {
		this.version = version;
	}
	public String getExploit() {
		return exploit;
	}
	public void setExploit(String exploit) {
		this.exploit = exploit;
	}
	public String getProjectName() {
		return projectName;
	}
	public void setProjectName(String projectName) {
		this.projectName = projectName;
	}
	public String getDisplayTargetFile() {
		return displayTargetFile;
	}
	public void setDisplayTargetFile(String displayTargetFile) {
		this.displayTargetFile = displayTargetFile;
	}
	public String getPublicId() {
		return publicId;
	}
	public void setPublicId(String publicId) {
		this.publicId = publicId;
	}
	public String getCustomUIContent() {
		return customUIContent;
	}
	public void setCustomUIContent(String customUIContent) {
		this.customUIContent = customUIContent;
	}
}