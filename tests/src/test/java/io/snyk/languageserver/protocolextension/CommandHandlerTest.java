package io.snyk.languageserver.protocolextension;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.nio.file.Path;
import java.util.List;

import org.eclipse.lsp4j.ExecuteCommandParams;
import org.eclipse.lsp4j.services.LanguageServer;
import org.eclipse.lsp4j.services.WorkspaceService;
import org.instancio.Instancio;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.languageserver.CommandHandler;
import io.snyk.languageserver.LsBaseTest;
import io.snyk.languageserver.LsConstants;
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
	void testIgnoreIssue() {
		Issue issue = Instancio.create(Issue.class);
		cut.ignoreIssue(issue);
		List<Object> args = List.of(".", "ignore", "--id="+issue.additionalData().ruleId());
		verifyExecuteCommand(args);
	}
	
	@Test
	void testMonitor() {
		Path path = Path.of(".");
		cut.monitorProject(path);
		List<Object> args = List.of(".", "monitor", "--all-projects");
		verifyExecuteCommand(args);
	}

	private void verifyExecuteCommand(List<Object> args) {
		ExecuteCommandParams params = new ExecuteCommandParams(LsConstants.COMMAND_SNYK_CLI, args);
		verify(wsMock).executeCommand(eq(params));
	}

}
