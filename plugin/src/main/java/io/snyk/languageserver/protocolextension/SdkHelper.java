package io.snyk.languageserver.protocolextension;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.apache.commons.lang3.exception.ExceptionUtils;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.lsp4j.WorkspaceFolder;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import io.snyk.eclipse.plugin.utils.ResourceUtils;
import io.snyk.eclipse.plugin.utils.SnykLogger;
import io.snyk.languageserver.protocolextension.messageObjects.LsSdk;

public class SdkHelper {
	public static final String PYDEV_NATURE = "org.python.pydev.pythonNature";
	private static final String PYDEV_PYTHON_INTERPRETER_PROP_NAME = "org.python.pydev.PYTHON_PROJECT_INTERPRETER";
	private static final String PYDEV_PREFERENCES_KEY = "org.python.pydev";
	private static final String JAVA = "java";
	private static final String RT_JAR_JAVA9_PLUS = "jrt-fs.jar";
	private static final String RT_JAR_JAVA3_PLUS = "rt.jar";

	public LsSdk getJDK(IProject project) {
		try {
			IJavaProject javaProject = JavaCore.create(project);
			IClasspathEntry[] classpathEntries;
			classpathEntries = javaProject.getResolvedClasspath(true);
			for (IClasspathEntry entry : classpathEntries) {
				if (entry.getEntryKind() == IClasspathEntry.CPE_LIBRARY) {
					IPath classPath = entry.getPath();
					var last = classPath.lastSegment().toLowerCase();

					if (last.contains(RT_JAR_JAVA9_PLUS) || last.contains(RT_JAR_JAVA3_PLUS)) {
						// the classpath contains the rt.jar, which is located in the lib folder
						// thus, we go two segments up in the path, to get the java home
						String javaHome = classPath.removeLastSegments(2).toOSString();
						return new LsSdk(JAVA, javaHome);
					}
				}
			}
		} catch (Exception e) {
			SnykLogger.logInfo(ExceptionUtils.getStackTrace(e));
			return null;
		}
		return null;
	}

	public LsSdk getPythonInterpreter(IProject project) {
		try {
			var name = getPyDevInterpreterName(ResourceUtils.getFullPath(project).toString());
			if (name == null) {
				return null;
			}
			return getPyDevExecutable(name);
		} catch (Exception e) {
			SnykLogger.logInfo(ExceptionUtils.getStackTrace(e));
			return null;
		}
	}

	private String getPyDevInterpreterName(String projectPath) throws IOException {
		File pydevProjectFile = Paths.get(projectPath, ".pydevproject").toFile();
		if (!pydevProjectFile.exists()) {
			return null;
		}
		try {
			DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
			DocumentBuilder builder = factory.newDocumentBuilder();
			Document document = builder.parse(pydevProjectFile);

			NodeList propertyList = document.getElementsByTagName("pydev_property");
			for (int i = 0; i < propertyList.getLength(); i++) {
				Element property = (Element) propertyList.item(i);
				String name = property.getAttribute("name");
				if (name.contains("INTERPRETER")) {
					return property.getTextContent();
				}
			}
		} catch (Exception e) {
			SnykLogger.logInfo(ExceptionUtils.getStackTrace(e));
		}
		return null;
	}

	private LsSdk getPyDevExecutable(String interpreterName) {
		IEclipsePreferences preferences = InstanceScope.INSTANCE.getNode(PYDEV_PREFERENCES_KEY);
		String interpretersPref = preferences.get("PYTHON_INTERPRETERS", "");
		String[] interpreters = interpretersPref.split(",");
		for (String interpreter : interpreters) {
			String[] parts = interpreter.split("=");
			if (parts[0].trim().equalsIgnoreCase(interpreterName)) {
				var path = parts[1].trim();
				LsSdk lsSdk = new LsSdk("python", path);
				return lsSdk;
			}
		}
		return null;
	}

	public List<LsSdk> getSdk(WorkspaceFolder workspaceFolder) {
		var uri = URI.create(workspaceFolder.getUri());
		var path = Path.of(uri);
		List<LsSdk> list = new ArrayList<>();
		var project = ResourceUtils.getProjectByPath(path);
		if (project == null)
			return list;
		try {
			LsSdk jdk = getJDK(project);
			if (jdk != null)
				list.add(jdk);
		} catch (Exception e) {
			SnykLogger.logInfo(ExceptionUtils.getStackTrace(e));
		}

		try {
			LsSdk pythonInterpreter = getPythonInterpreter(project);
			if (pythonInterpreter != null)
				list.add(pythonInterpreter);
		} catch (Exception e) {
			SnykLogger.logInfo(ExceptionUtils.getStackTrace(e));
		}
		return list;
	}
}
