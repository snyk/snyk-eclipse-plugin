package io.snyk.eclipse.plugin.views;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;

import io.snyk.eclipse.plugin.domain.ContentError;
import io.snyk.eclipse.plugin.domain.MonitorResult;
import io.snyk.eclipse.plugin.domain.ScanResult;
import io.snyk.eclipse.plugin.domain.Vuln;
import io.snyk.eclipse.plugin.exception.AuthException;
import io.snyk.eclipse.plugin.runner.Authenticator;
import io.snyk.eclipse.plugin.runner.ProcessResult;
import io.snyk.eclipse.plugin.runner.SnykCliRunner;

public class DataProvider {
	
	public static DataProvider INSTANCE = new DataProvider();

	public AtomicBoolean abort = new AtomicBoolean(false);
	private SnykCliRunner cliRunner = new SnykCliRunner();
	ObjectMapper objectMapper = new ObjectMapper().configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
		
		
	public List<DisplayModel> scanWorkspace() {
		List<IProject> allProjects = Arrays.asList(ResourcesPlugin.getWorkspace().getRoot().getProjects());
		return scan(allProjects);
	}
	
	public List<DisplayModel> scanProject(String projectName) {
		List<IProject> selectedProjects = Arrays.stream(ResourcesPlugin.getWorkspace().getRoot().getProjects())
				.filter(project -> project.getName().equals(projectName))
				.collect(Collectors.toList());
		return scan(selectedProjects);
	}
	
	public MonitorResult monitorProject(String projectName) {
		Optional<IProject> foundProject = Arrays.stream(ResourcesPlugin.getWorkspace().getRoot().getProjects())
				.filter(project -> project.getName().equals(projectName))
				.findAny();
		
		ProcessResult result = foundProject.map(this::monitor).orElse(ProcessResult.error("Unable to find project: " + projectName));
		return mapMonitorResult(result);
	}
	
	private ProcessResult monitor(IProject project) {
		if (project == null) return ProcessResult.error("project = null");
		IPath path = project.getRawLocation();
		
		if (path == null) {
			path = project.getLocation();
			if (path == null) {
				return ProcessResult.error("Unable to find project :" + project.getName());	
			}
		}		
		File location = new File(path.toString());

		ProcessResult result = cliRunner.snykMonitor(location);
		return result;
	}
	
	private MonitorResult mapMonitorResult(ProcessResult processResult) {
		if (processResult.hasError()) return MonitorResult.error(processResult.getError());
		else if (processResult.hasContentError())return MonitorResult.error(processResult.getContent());
		else
			try {
				return objectMapper.readValue(processResult.getContent(), MonitorResult.class);
			} catch (Exception e) {
				e.printStackTrace();
				return MonitorResult.error(e.getMessage());
			} 
	}
	
	
	public List<DisplayModel> scan(List<IProject> projects) {
		abort.set(false);
		List<DisplayModel> result = new ArrayList<>();

		try {
			Authenticator.INSTANCE.doAuthentication();
		} catch (AuthException e) {
			result.add(error("", e));
			return result;
		}
			
		for (IProject project : projects) {
			if (abort.get()) return abortResult();
			if (!project.isOpen()) continue;

			List<IFile> poms = scrapeForPomfiles(project);
			if (poms.size() > 1) {
				DisplayModel projectLevel = new DisplayModel();
				projectLevel.projectName = project.getName();
				projectLevel.description = project.getName();
				for (IFile pom : poms) {
					if (abort.get()) return abortResult();
					projectLevel.children.add(scanFile(pom, project.getName()));
				}
				result.add(projectLevel);
			} else {
				result.add(scanProject(project));
			}
		}
		return result;
	}
	
	public List<DisplayModel> abortResult() {
		System.out.println("abort!");
		List<DisplayModel> result = new ArrayList<>();
		result.add(message("scan aborted"));
		return result;
	}
	
	private DisplayModel scanFile(IFile file, String projectName) {
		ProcessResult result = cliRunner.snykTestFile(file.getRawLocation().toString());
		return processResult(result, projectName, Optional.of(file.getFullPath().toString()));
	}

	private DisplayModel scanProject(IProject project) {
		if (project == null) return new DisplayModel();
		IPath path = project.getRawLocation();
		
		if (path == null) {
			path = project.getLocation();
			if (path == null) {
				return DisplayModel.builder().description(project.getName()).children(new ArrayList<>()).build();	
			}
		}		
		File location = new File(path.toString());

		ProcessResult result = cliRunner.snykTest(location);
		return processResult(result, project.getName(), Optional.empty());
	}

	private DisplayModel processResult(ProcessResult result, String projectName, Optional<String> fileName) {
		try {
			System.out.println(result);
			DisplayModel projectModel;
			if (result.hasError()) {
				projectModel = DisplayModel.builder().description(result.getError()).projectName(projectName).build();
			} else if (result.hasContentError()) {
				ContentError error = objectMapper.readValue(result.getContent(), ContentError.class);
				projectModel = DisplayModel.builder()
						.description(projectName + " " + error.getError() + " Path: " + error.getPath())
						.projectName(projectName).build();
			} else {
				ScanResult scanResult = objectMapper.readValue(result.getContent(), ScanResult.class);
				List<DisplayModel> vulns = scanResult.getVulnerabilities().stream()
						.sorted(Comparator.comparingInt(vuln -> ((Vuln) vuln).getCvssScore()).reversed())
						.map(this::transform).collect(Collectors.toList());
				
				projectModel = DisplayModel.builder().description(fileName.orElse(projectName))
						.projectName(projectName)
						.dependecy(scanResult.getUniqueCount() + " vulns, " + scanResult.getSummary()).children(vulns)
						.build();
			}
			return projectModel;
		} catch (Exception e) {
			return error(projectName, e);
		}

	}

	List<IFile> scrapeForPomfiles(IProject project) {
		try {
			return processContainer(project, new ArrayList<>());
		} catch (CoreException e) {
			e.printStackTrace();
			return new ArrayList<>();
		}
	}

	List<IFile> processContainer(IContainer container, List<IFile> files) throws CoreException {
		IResource[] members = container.members();
		for (IResource member : members) {
			if (member instanceof IContainer)
				processContainer((IContainer) member, files);
			else if (member instanceof IFile)
				processFile((IFile) member).ifPresent(files::add);
		}
		return files;
	}

	Optional<IFile> processFile(IFile member) {
		if (member.getName().equals("pom.xml")) {
			return Optional.of(member);
		}
		return Optional.empty();
	}

	private DisplayModel transform(Vuln vuln) {
		List<String> vulns = vuln.getFrom().stream()
				.skip((vuln.getFrom().size() >= 2) ? 2 : 1)
				.collect(Collectors.toList());
		List<DisplayModel> pathtrace = IntStream.range(0, vulns.size())
				.mapToObj(i -> fromPath(vulns.get(i), i+1))
				.collect(Collectors.toList());
				
		return DisplayModel.builder().description(vuln.getTitle()).severity(vuln.getSeverity())
				.dependecy(vuln.getVulnTopLevelDependecy()).vulnPackage(vuln.getPackageName() + "@" + vuln.getVersion())
				.link(vuln.getUrl())
				.id(vuln.getId())
				.fix(vuln.getFix()).vulnPath(vuln.printFrom()).children(pathtrace).build();
	}
	

	private DisplayModel fromPath(String path, int indent) {
		String arrow = Stream.generate(() -> "-").limit(indent).collect(Collectors.joining()) + ">";
		return DisplayModel.builder().dependecy(arrow + " " + path).build();
	}
	
	public DisplayModel message(String message) {
		DisplayModel messageModel = new DisplayModel();
		messageModel.description = message;
		return messageModel;
	}

	public DisplayModel error(String prefix, Exception e) {
		e.printStackTrace();
		String message =  "Error: " + e.getMessage();
		if (!prefix.isEmpty()) message = prefix + " - " + message;
		return DisplayModel.builder().description(message).children(new ArrayList<>()).build();
	}

}
