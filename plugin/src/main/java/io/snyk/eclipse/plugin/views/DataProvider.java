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
import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.osgi.framework.Bundle;
import org.osgi.framework.FrameworkUtil;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.exc.MismatchedInputException;

import io.snyk.eclipse.plugin.domain.ContentError;
import io.snyk.eclipse.plugin.domain.MonitorResult;
import io.snyk.eclipse.plugin.domain.ScanResult;
import io.snyk.eclipse.plugin.domain.Vuln;
import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.runner.ProcessResult;
import io.snyk.eclipse.plugin.runner.SnykCliRunner;
import io.snyk.eclipse.plugin.utils.SnykLogger;

public class DataProvider {

  private static final Bundle BUNDLE = FrameworkUtil.getBundle(DataProvider.class);
  private static final ILog LOG = Platform.getLog(BUNDLE);

  public static final DataProvider INSTANCE = new DataProvider();

  public static final AtomicBoolean abort = new AtomicBoolean(false);
  private final SnykCliRunner cliRunner = new SnykCliRunner();
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

    return cliRunner.snykMonitor(location);
  }

  private MonitorResult mapMonitorResult(ProcessResult processResult) {
    if (processResult.hasError()) return MonitorResult.error(processResult.getError());
    else if (processResult.hasContentError()) return MonitorResult.error(processResult.getContent());
    else
      try {
        return objectMapper.readValue(processResult.getContent(), MonitorResult.class);
      } catch (Exception e) {
        SnykLogger.logError(e);
        return MonitorResult.error(e.getMessage());
      }
  }

  public List<DisplayModel> scan(List<IProject> projects) {
    abort.set(false);

    List<DisplayModel> result = new ArrayList<>();
    var additionalParams = Preferences.getInstance().getPref(Preferences.ADDITIONAL_PARAMETERS, "");
    for (IProject project : projects) {
      if (abort.get()) return abortResult();
      if (!project.isOpen()) continue;

      List<IFile> poms = scrapeForPomfiles(project);
      if (poms.size() > 0) {
        if (!additionalParams.contains("--all-projects")) {
          var tempParams = additionalParams + " --all-projects".trim();
          Preferences.getInstance().store(Preferences.ADDITIONAL_PARAMETERS, tempParams);
        }
      }
      result.add(scanProject(project));
      Preferences.getInstance().store(Preferences.ADDITIONAL_PARAMETERS, additionalParams);
    }
    return result;
  }

  public List<DisplayModel> abortResult() {
    List<DisplayModel> result = new ArrayList<>();
    result.add(message("scan aborted"));
    return result;
  }

  private IPath getPath(IProject project) {
    IPath path = project.getRawLocation();

    if (path == null) {
      path = project.getLocation();

    }
    return path;
  }

  private DisplayModel scanProject(IProject project) {
    if (project == null) return new DisplayModel();
    IPath path = getPath(project);
    if (path == null) {
      return DisplayModel.builder().description(project.getName()).children(new ArrayList<>()).iProject(project)
        .build();
    }

    File location = new File(path.toString());
    ProcessResult result = cliRunner.snykTest(location);
    return processResult(result, project);
  }

  private DisplayModel processResult(ProcessResult result, IProject project) {
    String projectName = project.getName();

    IStatus[] statuses = new IStatus[]{
      new Status(Status.INFO, BUNDLE.getSymbolicName(), "result = " + result.getContent())};
    MultiStatus multiStatusResult = new MultiStatus(BUNDLE.getSymbolicName(), Status.INFO, statuses,
      "Snyk command result", null);
    LOG.log(multiStatusResult);

    try {
      DisplayModel projectModel;
      if (result.hasError()) {
        projectModel = DisplayModel.builder().description(result.getError()).projectName(projectName).build();
      } else if (result.hasContentError()) {
        ContentError error = objectMapper.readValue(result.getContent(), ContentError.class);
        projectModel = DisplayModel.builder()
          .description(projectName + " " + error.getError() + " Path: " + error.getPath()).projectName(projectName)
          .build();
      } else {
        ScanResult[] scanResults = parseScanResults(result);
        projectModel = getProjectModel(project, projectName, scanResults);
      }
      return projectModel;
    } catch (Exception e) {
      return error(projectName, e);
    }
  }

  private ScanResult[] parseScanResults(ProcessResult result) throws JsonProcessingException {
    var json = result.getContent();
    ScanResult[] scanResults;
    try {
      scanResults = objectMapper.readValue(json, ScanResult[].class);
    } catch (MismatchedInputException e) {
      ScanResult scanResult = objectMapper.readValue(result.getContent(), ScanResult.class);
      scanResults = new ScanResult[]{scanResult};
    }
    return scanResults;
  }

  private DisplayModel getProjectModel(IProject project, String projectName, ScanResult[] scanResults) {
    DisplayModel projectModel;
    List<DisplayModel> scanResultModels = new ArrayList<>();
    for (ScanResult scanResult : scanResults) {
      List<DisplayModel> vulns = scanResult.getVulnerabilities().stream()
        .sorted(Comparator.comparingInt(vuln -> ((Vuln) vuln).getCvssScore()).reversed())
        .map(vuln -> transform(vuln, project)).collect(Collectors.toList());

      var scanResultModel = DisplayModel.builder()
        .description(scanResult.getDisplayTargetFile())
        .projectName(scanResult.getDisplayTargetFile())
        .dependecy(scanResult.getUniqueCount() + " vulns, " + scanResult.getSummary()).children(vulns).build();
      scanResultModels.add(scanResultModel);
    }
    if (scanResultModels.size() > 1) {
      projectModel = DisplayModel.builder()
        .description(projectName)
        .projectName(projectName)
        .children(scanResultModels).build();
    } else {
      projectModel = scanResultModels.get(0);
      projectModel.description = projectName;
      projectModel.iProject = project;
      projectModel.projectName = projectName;
    }
    return projectModel;
  }

  List<IFile> scrapeForPomfiles(IProject project) {
    try {
      return processContainer(project, new ArrayList<>());
    } catch (CoreException e) {
      SnykLogger.logError(e);
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

  private DisplayModel transform(Vuln vuln, IProject project) {
    List<String> vulns = vuln.getFrom().stream()
      .skip((vuln.getFrom().size() >= 2) ? 2 : 1)
      .collect(Collectors.toList());
    List<DisplayModel> pathtrace = IntStream.range(0, vulns.size())
      .mapToObj(i -> fromPath(vulns.get(i), i + 1))
      .collect(Collectors.toList());

    return DisplayModel.builder().description(vuln.getTitle()).severity(vuln.getSeverity())
      .dependecy(vuln.getVulnTopLevelDependecy()).vulnPackage(vuln.getPackageName() + "@" + vuln.getVersion())
      .link(vuln.getUrl())
      .id(vuln.getId())
      .iProject(project)
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
    String message = "Error: " + e.getMessage();
    if (!prefix.isEmpty()) message = prefix + " - " + message;
    return DisplayModel.builder().description(message).children(new ArrayList<>()).build();
  }

  public void ignoreIssue(String id, IProject project) {
    IPath path = getPath(project);

    ProcessResult ignoreResult = cliRunner.snykIgnore(id, new File(path.toString()));
    messageProcessResult(ignoreResult, "Ignoring " + id + " failed", "Ignoring " + id + " for this project for 30 days");
  }

  public static void messageProcessResult(ProcessResult result, String errorMessage, String okMessage) {
    Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();

    if (result.hasErrorOrContentError())
      shell.getDisplay().asyncExec(() -> MessageDialog.openError(shell, errorMessage, result.getErrorOrContent()));
    else shell.getDisplay().asyncExec(() -> MessageDialog.openInformation(shell, "", okMessage));
  }

  public static void popUpWarn(String title, String message) {
    Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
    shell.getDisplay().asyncExec(() -> MessageDialog.openWarning(shell, title, message));
  }

}
