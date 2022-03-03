package io.snyk.languageserver.download;

import org.apache.commons.lang3.SystemUtils;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import static io.snyk.eclipse.plugin.utils.FileSystemUtil.getCliDirectory;

public class LsUtils {
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
		return map.get(os.toLowerCase().substring(0,3));
	}
    
    public String getBinaryName() {
    	var osName = SystemUtils.OS_NAME;
    	var executable = "snyk-ls";
        if (osName.toLowerCase().startsWith("win")) executable += ".exe";
        return executable;
    }
    
    public File getLSFile() {
        return new File(getCliDirectory(), getBinaryName());
    }
}
