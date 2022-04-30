package io.snyk.languageserver;

import io.snyk.eclipse.plugin.Activator;
import io.snyk.eclipse.plugin.properties.Preferences;
import org.apache.commons.lang3.SystemUtils;
import org.eclipse.core.net.proxy.IProxyData;
import org.eclipse.core.net.proxy.IProxyService;
import org.eclipse.core.runtime.Platform;
import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceReference;

import java.io.File;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import static io.snyk.eclipse.plugin.utils.FileSystemUtil.getCliDirectory;

public class LsRuntimeEnvironment {
  private final Preferences preferences;

  public Preferences getPreferences() {
    return preferences;
  }

  public static final Map<String, String> map = new HashMap<>();

  static {
    map.put("win", "windows");
    map.put("lin", "linux");
    map.put("mac", "darwin");
    map.put("dar", "darwin");

    map.put("amd64", "amd64");
    map.put("x8664", "amd64");
    map.put("x86_64", "amd64");
    map.put("x64", "amd64");

    map.put("aarch_64", "arm64");
    map.put("arm64", "arm64");

    map.put("arm_32", "arm");
    map.put("arm32", "arm");
    map.put("arm", "arm");

    map.put("x8632", "386");
    map.put("x86_32", "386");
    map.put("x86", "386");
    map.put("ia32", "386");
    map.put("486", "386");
    map.put("586", "386");
    map.put("686", "386");
  }

  public LsRuntimeEnvironment(Preferences prefs) {
    this.preferences = prefs;
  }

  public String getDownloadBinaryName(String version) {
    String base = "snyk-ls_%s_%s_%s";
    String os = getOs();
    String executable = String.format(base, version, os, getArch());
    if (executable.toLowerCase().contains("windows")) executable += ".exe";
    return executable;
  }

  String getArch() {
    String arch = SystemUtils.OS_ARCH;
    return map.get(arch.toLowerCase());
  }

  String getOs() {
    String os = SystemUtils.OS_NAME;
    return map.get(os.toLowerCase().substring(0, 3));
  }

  public String getBinaryName() {
    var osName = SystemUtils.OS_NAME;
    var executable = "snyk-ls";
    if (osName.toLowerCase().startsWith("win")) executable += ".exe";
    return executable;
  }

  public File getLSFile() {
    String lsBinaryPath = preferences.getLsBinary();
    File binary = new File(getCliDirectory(), getBinaryName());
    if (lsBinaryPath != null && !lsBinaryPath.isBlank()) {
      binary = new File(lsBinaryPath);
    }
    return binary;
  }

  public void updateEnvironment(Map<String, String> env) {
    String authToken = preferences.getAuthToken();
    if (authToken != null && !authToken.isBlank()) {
      env.put("SNYK_TOKEN", authToken);
    }
    String endpoint = preferences.getEndpoint();
    if (endpoint != null && !endpoint.isEmpty()) {
      env.put("SNYK_API", endpoint);
    }
    addPath(env);
    addIntegrationInfoToEnv(env);
    addProxyToEnv(env);
    addProductEnablement(env);
    addAdditionalParamsAndEnv(env);
  }

  void addAdditionalParamsAndEnv(Map<String, String> env) {
    String additionalParams = preferences.getPref(Preferences.ADDITIONAL_PARAMETERS, "");
    if (additionalParams != null && !additionalParams.isBlank()) {
      env.put(Preferences.ADDITIONAL_PARAMETERS, additionalParams);
    }

    String additionalEnv = preferences.getPref(Preferences.ADDITIONAL_ENVIRONMENT, "");
    if (additionalEnv != null && !additionalEnv.isBlank()) {
      while (additionalEnv.endsWith(";")) {
        additionalEnv = additionalEnv.substring(0, additionalEnv.length() - 1);
      }
      var variables = additionalEnv.split(";");
      for (String variable : variables) {
        var split = variable.split("=");
        if (split.length == 2) {
          env.put(split[0], split[1]);
        }
      }
    }
  }

  void addProductEnablement(Map<String, String> env) {
    env.put(Preferences.ACTIVATE_SNYK_CODE, preferences.getPref(Preferences.ACTIVATE_SNYK_CODE));
    env.put(Preferences.ACTIVATE_SNYK_IAC, preferences.getPref(Preferences.ACTIVATE_SNYK_IAC));
    env.put(Preferences.ACTIVATE_SNYK_OPEN_SOURCE, preferences.getPref(Preferences.ACTIVATE_SNYK_OPEN_SOURCE));
  }

  private void addPath(Map<String, String> env) {
    var pathOptional = preferences.getPath();
    if (pathOptional.isPresent()) {
      var path = pathOptional.get();
      String splitBy;
      if (path.contains(";")) {
        splitBy = ";";
      } else {
        splitBy = ":";
      }
      env.put("PATH", path.replace(splitBy, File.pathSeparator) + File.pathSeparator + System.getenv("PATH"));
    }
  }

  private void addProxyToEnv(Map<String, String> env) {
    for (Entry<Object, Object> entry : System.getProperties().entrySet()) {
      if (!(entry.getKey() instanceof String && entry.getValue() instanceof String)) continue;

      String property = (String) entry.getKey();
      if (property.toLowerCase().contains("proxy") || property.toLowerCase().contains("proxies")) {
        env.put(property, (String) entry.getValue());
      }
    }
    IProxyService service = getProxyService();
    if (service == null) return;
    for (IProxyData data : service.getProxyData()) {
      if (data.getHost() == null) continue;

      String userId = data.getUserId();
      String creds = "";
      if (userId != null && !userId.isBlank()) {
        creds = userId;
        String password = data.getPassword();
        if (password != null && !password.isBlank()) {
          creds += ":" + password;
        }
        creds += "@";
      }
      String value = creds + data.getHost() + ":" + data.getPort();
      env.put(data.getType().toLowerCase() + "_proxy", value);
    }
    String[] nonProxiedHostsArray = service.getNonProxiedHosts();
    if (nonProxiedHostsArray != null && nonProxiedHostsArray.length > 0) {
      String nonProxiedHosts = String.join(",", nonProxiedHostsArray);
      if (!nonProxiedHosts.isEmpty()) {
        env.put("no_proxy", nonProxiedHosts);
      }
    }
  }

  public IProxyService getProxyService() {
    BundleContext bundleContext = Platform.getBundle(Activator.PLUGIN_ID).getBundleContext();
    ServiceReference<IProxyService> serviceRef = bundleContext.getServiceReference(IProxyService.class);

    if (serviceRef == null) return null;

    return bundleContext.getService(serviceRef);
  }

  private void addIntegrationInfoToEnv(Map<String, String> env) {
    env.put("SNYK_INTEGRATION_NAME", Activator.INTEGRATION_NAME);
    env.put("SNYK_INTEGRATION_VERSION", Activator.PLUGIN_VERSION);
  }
}
