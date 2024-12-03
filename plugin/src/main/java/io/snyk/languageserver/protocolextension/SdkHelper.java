package io.snyk.languageserver.protocolextension;

import org.apache.commons.lang3.exception.ExceptionUtils;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;

import io.snyk.eclipse.plugin.utils.SnykLogger;
import io.snyk.languageserver.protocolextension.messageObjects.LsSdk;

public class SdkHelper {
	private static final String JAVA = "java";

	public LsSdk getJDK(IProject project) {
		// java
		IJavaProject javaProject = JavaCore.create(project);
		IClasspathEntry[] classpathEntries;
		try {
			classpathEntries = javaProject.getResolvedClasspath(true);
			for (IClasspathEntry entry : classpathEntries) {
				if (entry.getEntryKind() == IClasspathEntry.CPE_LIBRARY) {
					IPath classPath = entry.getPath();
					var segments = classPath.segments();
					for (String segment : segments) {
						if (segment.toLowerCase().contains("jdk") || segment.toLowerCase().contains("jre")) {
							String javaHome = classPath.removeLastSegments(2).toOSString();
							return new LsSdk(JAVA, javaHome);
						}
					}
				}
			}
		} catch (Exception e) {
			SnykLogger.logInfo(ExceptionUtils.getStackTrace(e));
			return null;
		}
		return null;
	}

}
