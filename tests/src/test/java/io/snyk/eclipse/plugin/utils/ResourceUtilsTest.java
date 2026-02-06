package io.snyk.eclipse.plugin.utils;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

import java.nio.file.Path;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class ResourceUtilsTest {

	private IProject createMockProject(String name, String absolutePath) {
		IProject project = mock(IProject.class);
		when(project.getName()).thenReturn(name);
		when(project.isAccessible()).thenReturn(true);
		when(project.isDerived()).thenReturn(false);
		when(project.isHidden()).thenReturn(false);

		IPath location = mock(IPath.class);
		when(location.toPath()).thenReturn(Path.of(absolutePath));
		when(project.getLocation()).thenReturn(location);

		return project;
	}

	@Test
	void testIndependentProjectsAreAllReturned() {
		IProject projectA = createMockProject("GCASharedServicesMain",
				"/Users/test/github/repo-apc-common/GCASharedServicesMain");
		IProject projectB = createMockProject("ei.core.service",
				"/Users/test/github/repo-core-services/ei.core.service");
		IProject projectC = createMockProject("gc-ap-portfolio-foundation-service",
				"/Users/test/github/repo-portfolio-foundation-service/gc-ap-portfolio-foundation-service");

		IWorkspaceRoot root = mock(IWorkspaceRoot.class);
		when(root.getProjects()).thenReturn(new IProject[] { projectA, projectB, projectC });

		IWorkspace workspace = mock(IWorkspace.class);
		when(workspace.getRoot()).thenReturn(root);

		try (MockedStatic<ResourcesPlugin> resourcesPluginMock = mockStatic(ResourcesPlugin.class);
				MockedStatic<SnykLogger> loggerMock = mockStatic(SnykLogger.class)) {
			resourcesPluginMock.when(ResourcesPlugin::getWorkspace).thenReturn(workspace);

			List<IProject> result = ResourceUtils.getAccessibleTopLevelProjects();

			assertEquals(3, result.size(), "All 3 independent projects should be returned");
			assertTrue(result.contains(projectA));
			assertTrue(result.contains(projectB));
			assertTrue(result.contains(projectC));
		}
	}

	@Test
	void testSubProjectIsFilteredOut() {
		IProject parent = createMockProject("parent", "/Users/test/repos/parent");
		IProject child = createMockProject("child", "/Users/test/repos/parent/modules/child");

		IWorkspaceRoot root = mock(IWorkspaceRoot.class);
		when(root.getProjects()).thenReturn(new IProject[] { parent, child });

		IWorkspace workspace = mock(IWorkspace.class);
		when(workspace.getRoot()).thenReturn(root);

		try (MockedStatic<ResourcesPlugin> resourcesPluginMock = mockStatic(ResourcesPlugin.class);
				MockedStatic<SnykLogger> loggerMock = mockStatic(SnykLogger.class)) {
			resourcesPluginMock.when(ResourcesPlugin::getWorkspace).thenReturn(workspace);

			List<IProject> result = ResourceUtils.getAccessibleTopLevelProjects();

			assertEquals(1, result.size(), "Only the parent project should be returned");
			assertTrue(result.contains(parent));
		}
	}

	@Test
	void testProjectAfterSubProjectIsNotDropped() {
		// This is the exact bug scenario from IDE-1083:
		// After sorting by path, if a sub-project sets add=false and the flag is never
		// reset, all subsequent independent projects are silently dropped.
		IProject parent = createMockProject("parent", "/Users/test/repos/aaa-parent");
		IProject child = createMockProject("child", "/Users/test/repos/aaa-parent/modules/child");
		IProject independent = createMockProject("independent", "/Users/test/repos/zzz-independent");

		IWorkspaceRoot root = mock(IWorkspaceRoot.class);
		when(root.getProjects()).thenReturn(new IProject[] { parent, child, independent });

		IWorkspace workspace = mock(IWorkspace.class);
		when(workspace.getRoot()).thenReturn(root);

		try (MockedStatic<ResourcesPlugin> resourcesPluginMock = mockStatic(ResourcesPlugin.class);
				MockedStatic<SnykLogger> loggerMock = mockStatic(SnykLogger.class)) {
			resourcesPluginMock.when(ResourcesPlugin::getWorkspace).thenReturn(workspace);

			List<IProject> result = ResourceUtils.getAccessibleTopLevelProjects();

			assertEquals(2, result.size(), "Parent and independent project should both be returned");
			assertTrue(result.contains(parent), "Parent project must be present");
			assertTrue(result.contains(independent), "Independent project must not be dropped after sub-project filtering");
		}
	}

	@Test
	void testInaccessibleProjectIsFiltered() {
		IProject accessible = createMockProject("accessible", "/Users/test/repos/accessible");
		IProject inaccessible = createMockProject("inaccessible", "/Users/test/repos/inaccessible");
		when(inaccessible.isAccessible()).thenReturn(false);

		IWorkspaceRoot root = mock(IWorkspaceRoot.class);
		when(root.getProjects()).thenReturn(new IProject[] { accessible, inaccessible });

		IWorkspace workspace = mock(IWorkspace.class);
		when(workspace.getRoot()).thenReturn(root);

		try (MockedStatic<ResourcesPlugin> resourcesPluginMock = mockStatic(ResourcesPlugin.class);
				MockedStatic<SnykLogger> loggerMock = mockStatic(SnykLogger.class)) {
			resourcesPluginMock.when(ResourcesPlugin::getWorkspace).thenReturn(workspace);

			List<IProject> result = ResourceUtils.getAccessibleTopLevelProjects();

			assertEquals(1, result.size());
			assertTrue(result.contains(accessible));
		}
	}

	@Test
	void testDerivedProjectIsFiltered() {
		IProject normal = createMockProject("normal", "/Users/test/repos/normal");
		IProject derived = createMockProject("derived", "/Users/test/repos/derived");
		when(derived.isDerived()).thenReturn(true);

		IWorkspaceRoot root = mock(IWorkspaceRoot.class);
		when(root.getProjects()).thenReturn(new IProject[] { normal, derived });

		IWorkspace workspace = mock(IWorkspace.class);
		when(workspace.getRoot()).thenReturn(root);

		try (MockedStatic<ResourcesPlugin> resourcesPluginMock = mockStatic(ResourcesPlugin.class);
				MockedStatic<SnykLogger> loggerMock = mockStatic(SnykLogger.class)) {
			resourcesPluginMock.when(ResourcesPlugin::getWorkspace).thenReturn(workspace);

			List<IProject> result = ResourceUtils.getAccessibleTopLevelProjects();

			assertEquals(1, result.size());
			assertTrue(result.contains(normal));
		}
	}

	@Test
	void testHiddenProjectIsFiltered() {
		IProject visible = createMockProject("visible", "/Users/test/repos/visible");
		IProject hidden = createMockProject("hidden", "/Users/test/repos/hidden");
		when(hidden.isHidden()).thenReturn(true);

		IWorkspaceRoot root = mock(IWorkspaceRoot.class);
		when(root.getProjects()).thenReturn(new IProject[] { visible, hidden });

		IWorkspace workspace = mock(IWorkspace.class);
		when(workspace.getRoot()).thenReturn(root);

		try (MockedStatic<ResourcesPlugin> resourcesPluginMock = mockStatic(ResourcesPlugin.class);
				MockedStatic<SnykLogger> loggerMock = mockStatic(SnykLogger.class)) {
			resourcesPluginMock.when(ResourcesPlugin::getWorkspace).thenReturn(workspace);

			List<IProject> result = ResourceUtils.getAccessibleTopLevelProjects();

			assertEquals(1, result.size());
			assertTrue(result.contains(visible));
		}
	}
}
