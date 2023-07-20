package io.snyk.languageserver;

import io.snyk.eclipse.plugin.Activator;
import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import org.apache.commons.lang3.SystemUtils;
import org.eclipse.core.net.proxy.IProxyData;
import org.eclipse.core.net.proxy.IProxyService;
import org.eclipse.core.runtime.Platform;
import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceReference;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import static org.eclipse.core.net.proxy.IProxyData.HTTPS_PROXY_TYPE;
import static org.eclipse.core.net.proxy.IProxyData.HTTP_PROXY_TYPE;

public class LsRuntimeEnvironment {
  public static final Map<String, String> map = new HashMap<>();

  static {
    map.put("win", "windows");
    map.put("lin", "linux");
    map.put("mac", "darwin");
    map.put("dar", "darwin");
    map.put("macosx", "darwin");

    map.put("amd64", "amd64");
    map.put("x8664", "amd64");
    map.put("x86_64", "amd64");
    map.put("x64", "amd64");

    map.put("aarch_64", "arm64");
    map.put("aarch64", "arm64");
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

  public LsRuntimeEnvironment() {
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

  public void updateEnvironment(Map<String, String> env) {
    addIntegrationInfoToEnv(env);
    addProxyToEnv(env);
    addProductEnablement(env);
    addOrganization(env);
    addAdditionalParamsAndEnv(env);
    addTelemetry(env);
  }

  void addOrganization(Map<String, String> env) {
    String pref = Preferences.getInstance().getPref(Preferences.ORGANIZATION_KEY, "");
    if (pref != null && !pref.isBlank()) {
      env.put(Preferences.ORGANIZATION_KEY, pref);
    }
  }

  public void addAdditionalParamsAndEnv(Map<String, String> env) {
    String additionalParams = Preferences.getInstance().getPref(Preferences.ADDITIONAL_PARAMETERS, "");
    if (additionalParams != null && !additionalParams.isBlank()) {
      env.put(Preferences.ADDITIONAL_PARAMETERS, additionalParams);
    }

    String additionalEnv = Preferences.getInstance().getPref(Preferences.ADDITIONAL_ENVIRONMENT, "");
    if (additionalEnv != null && !additionalEnv.isBlank()) {
      while (additionalEnv.endsWith(";")) {
        additionalEnv = additionalEnv.substring(0, additionalEnv.length() - 1);
      }
      var variables = additionalEnv.split(";");
      for (String variable : variables) {
        var split = variable.split("=");
        if (split.length > 1) {
          String name = split[0];
          List<String> value = new ArrayList<>(Arrays.asList(split));
          value.remove(0);
          env.put(name, String.join("=", value)); // allow equal signs in variable values
        }
      }
    }
  }

  void addProductEnablement(Map<String, String> env) {
    env.put(Preferences.ACTIVATE_SNYK_CODE, Preferences.getInstance().getPref(Preferences.ACTIVATE_SNYK_CODE));
    env.put(Preferences.ACTIVATE_SNYK_IAC, Preferences.getInstance().getPref(Preferences.ACTIVATE_SNYK_IAC));
    env.put(Preferences.ACTIVATE_SNYK_OPEN_SOURCE, Preferences.getInstance().getPref(Preferences.ACTIVATE_SNYK_OPEN_SOURCE));
  }

  public void addPath(Map<String, String> env) {
    var pathOptional = Preferences.getInstance().getPath();
    if (pathOptional.isPresent()) {
      var path = pathOptional.get();
      String newPath = path + File.pathSeparator
        + System.getenv("PATH");
		env.put("PATH", newPath);
    }
  }

  public void addProxyToEnv(Map<String, String> env) {
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
      String protocol = data.getType().toLowerCase();
      if (data.getType().equals(HTTPS_PROXY_TYPE)) {
        // TODO verify correctness of this!
        protocol = HTTP_PROXY_TYPE.toLowerCase();
      }
      // TODO urlencode creds
      String value = protocol + "://" + creds + data.getHost() + ":" + data.getPort();
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

  public void addIntegrationInfoToEnv(Map<String, String> env) {
    env.put("SNYK_INTEGRATION_NAME", Activator.INTEGRATION_NAME);
    env.put("SNYK_INTEGRATION_VERSION", Activator.PLUGIN_VERSION);
  }

  void addTelemetry(Map<String, String> env) {
    String sendErrorReports = Preferences.getInstance().getPref(Preferences.SEND_ERROR_REPORTS, "true");
    env.put("SEND_ERROR_REPORTS", sendErrorReports);
    String enableTelemetry = Preferences.getInstance().getPref(Preferences.ENABLE_TELEMETRY, "false");
    // This is a bit confusing - CLI takes DISABLE as env variable, but we ask for ENABLE, so it's reverted
    if (Boolean.parseBoolean(enableTelemetry)) {
      env.put(Preferences.ENABLE_TELEMETRY, "0");
    } else {
      env.put(Preferences.ENABLE_TELEMETRY, "1");
    }
  }
}
