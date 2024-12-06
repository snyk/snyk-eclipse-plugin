package io.snyk.languageserver.protocolextension;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

import org.eclipse.lsp4j.ExecuteCommandParams;
import org.eclipse.lsp4j.services.LanguageServer;
import org.eclipse.lsp4j.services.WorkspaceService;
import org.instancio.Instancio;
import org.instancio.Select;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import io.snyk.eclipse.plugin.domain.ProductConstants;
import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.languageserver.CommandHandler;
import io.snyk.languageserver.LsBaseTest;
import io.snyk.languageserver.LsConstants;
import io.snyk.languageserver.protocolextension.messageObjects.scanResults.AdditionalData;
import io.snyk.languageserver.protocolextension.messageObjects.scanResults.Issue;

public class CommandHandlerTest extends LsBaseTest {
	private Preferences pref;
	private LanguageServer lsMock;
	private CommandHandler cut;
	private WorkspaceService wsMock;

	@BeforeEach
	protected void setUp() {
		super.setUp();
		pref = Preferences.getInstance();
		// we don't want the wizard to pop up, so we set a dummy token
		pref.store(Preferences.AUTH_TOKEN_KEY, "dummy");
		pref.store(Preferences.MANAGE_BINARIES_AUTOMATICALLY, "false");
		lsMock = Mockito.mock(LanguageServer.class);

		cut = new CommandHandler(lsMock);

		wsMock = Mockito.mock(WorkspaceService.class);
		when(lsMock.getWorkspaceService()).thenReturn(wsMock);
	}

	@AfterEach
	protected void tearDown() {
		super.tearDown();
	}

	@Test
	void testIgnore_CodeIssue_ignoreNotCalled() {
		Issue issue = getIssue(ProductConstants.FILTERABLE_ISSUE_CODE_SECURITY);

		cut.ignoreIssue(issue);

		verifyNoMoreInteractions(lsMock);
	}
	
	@Test
	void testIgnore_OSSIssue_pathIsNotAdded() {
		Issue issue = getIssue(ProductConstants.FILTERABLE_ISSUE_OPEN_SOURCE);

		cut.ignoreIssue(issue);
		
		List<Object> args = List.of(".", "ignore", "--id=" + issue.additionalData().ruleId());
		verifyExecuteCommand(args);
	}

	@Test
	void testIgnore_IaCIssue_pathIsAdded() {
		Issue issue = getIssue(ProductConstants.FILTERABLE_ISSUE_INFRASTRUCTURE_AS_CODE);
		
		var expectedPath = "file > a > b > c";
		

		cut.ignoreIssue(issue);
		
		List<Object> args = List.of(".", "ignore", "--path="+expectedPath, "--id=" + issue.additionalData().publicId());
		verifyExecuteCommand(args);
	}

	@Test
	void testMonitor() {
		Path path = Path.of(".");
		cut.monitorProject(path);
		List<Object> args = List.of(".", "monitor", "--all-projects");
		verifyExecuteCommand(args);
	}

	private Issue getIssue(String filterableIssueType) {
		List<String> path = List.of("a", "b", "c");
		var additionalData = Instancio.of(AdditionalData.class).set(Select.field(AdditionalData::path), path).create();
		Issue issue = Instancio.of(Issue.class)
				.set(Select.field(Issue::additionalData), additionalData)
				.set(Select.field(Issue::additionalData), additionalData)
				.set(Select.field(Issue::filterableIssueType), filterableIssueType)
				.set(Select.field(Issue::filePath), Paths.get(".", "file").toString())
				.create();
		return issue;
	}
	
	
	private void verifyExecuteCommand(List<Object> args) {
		ExecuteCommandParams params = new ExecuteCommandParams(LsConstants.COMMAND_SNYK_CLI, args);
		verify(wsMock).executeCommand(eq(params));
	}

}
