package io.snyk.eclipse.plugin.utils;

import java.io.ByteArrayOutputStream;
import java.net.URL;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;
import java.util.Comparator;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.FileLocator;
import org.osgi.framework.Bundle;

public class ResourceUtils {

	private static final Comparator<IProject> projectByPathComparator = new Comparator<IProject>() {
		@Override
		public int compare(IProject o1, IProject o2) {
			Path fullPath = ResourceUtils.getFullPath(o1);
			Path fullPath2 = ResourceUtils.getFullPath(o2);
			return fullPath.compareTo(fullPath2);
		}
	};

	private ResourceUtils() {
	}

	public static String getBase64Image(Bundle bundle, String icon) {
		URL imageUrl = FileLocator.find(bundle, new org.eclipse.core.runtime.Path("icons/" + icon), null);

		byte[] imageData = getImageDataFromUrl(imageUrl);

		String base64Image = Base64.getEncoder().encodeToString(imageData);
		return base64Image;
	}

	private static byte[] getImageDataFromUrl(URL imageUrl) {
		try (final var openStream = imageUrl.openStream()) {
			ByteArrayOutputStream output = new ByteArrayOutputStream();
			openStream.transferTo(output);
			return output.toByteArray();
		} catch (Exception e) {
			SnykLogger.logError(e);
			return new byte[0];
		}
	}

	public static Path getFullPath(IResource resource) {
		final var location = resource.getLocation();
		if (location == null) {
			return null;
		}
		return location.toPath().normalize().toAbsolutePath();
	}

	public static IProject getProjectByPath(Path path) {
		var projects = getAccessibleTopLevelProjects();

		for (IProject iProject : projects) {
			Path projectPath = ResourceUtils.getFullPath(iProject);
			if (path.normalize().startsWith(projectPath)) {
				return iProject;
			}
		}
		return null;
	}

	public static List<IProject> getAccessibleTopLevelProjects() {
		var projects = Arrays.stream(ResourcesPlugin.getWorkspace().getRoot().getProjects()).filter((project) -> {
			return project.isAccessible() && !project.isDerived() && !project.isHidden();
		}).sorted(projectByPathComparator).collect(Collectors.toList());

		Set<IProject> topLevel = new TreeSet<>(projectByPathComparator);
		boolean add = true;
		for (IProject iProject : projects) {
			var projectPath = ResourceUtils.getFullPath(iProject);
			for (IProject tp : topLevel) {
				var topLevelPath = ResourceUtils.getFullPath(tp);
				if (projectPath.startsWith(topLevelPath)) {
					add = false;
					break;
				}
			}
			if (add) {
				topLevel.add(iProject);
			}
		}

		return new ArrayList<>(topLevel);
	}
}
