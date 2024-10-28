package io.snyk.eclipse.plugin.utils;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import io.snyk.languageserver.protocolextension.messageObjects.LsSdk;

public class SdkHelper {
	

    private static List<LsSdk> retrieveJavaSDKs() {
        List<LsSdk> sdks = new ArrayList<>();
        try {
        	// we don't want to depend on the plugin, so we use reflection
            Class<?> javaRuntimeClass = Class.forName("org.eclipse.jdt.launching.JavaRuntime");
            Object[] vmInstalls = (Object[]) javaRuntimeClass.getMethod("getVMInstallTypes").invoke(null);
            
            for (Object vmInstall : vmInstalls) {
                File location = (File) vmInstall.getClass().getMethod("getInstallLocation").invoke(vmInstall);
                sdks.add(new LsSdk("Java", location.getAbsolutePath()));
            }
        } catch (ClassNotFoundException e) {
            System.out.println("JDT plugin not available. Unable to retrieve Java SDKs.");
        } catch (Exception e) {
            e.printStackTrace();
        }
        return sdks;
    }

    private static List<LsSdk> retrievePythonSDKs() {
        List<LsSdk> sdks = new ArrayList<>();
        try {
        	// we don't want to depend on the plugin, so we use reflection
            Class<?> pydevPluginClass = Class.forName("org.python.pydev.plugin.PydevPlugin");
            Object pythonInterpreterManager = pydevPluginClass.getMethod("getPythonInterpreterManager").invoke(null);
            String[] interpreterNames = (String[]) pythonInterpreterManager.getClass().getMethod("getInterpreterNames").invoke(pythonInterpreterManager);
            
            for (String interpreterName : interpreterNames) {
                Object interpreterInfo = pythonInterpreterManager.getClass().getMethod("getInterpreterInfo", String.class).invoke(pythonInterpreterManager, interpreterName);
                String executableOrJar = (String) interpreterInfo.getClass().getMethod("getExecutableOrJar").invoke(interpreterInfo);
                sdks.add(new LsSdk("Python", executableOrJar));
            }
        } catch (ClassNotFoundException e) {
            System.out.println("PyDev plugin not available. Unable to retrieve Python SDKs.");
        } catch (Exception e) {
            e.printStackTrace();
        }
        return sdks;
    }
}