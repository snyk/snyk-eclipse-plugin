package snykeclipseplugin.handlers;

import javax.inject.Named;

import org.eclipse.e4.core.di.annotations.Execute;
import org.eclipse.e4.ui.services.IServiceConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.resources.IProject;

/** <b>Warning</b> : 
As explained in <a href="http://wiki.eclipse.org/Eclipse4/RCP/FAQ#Why_aren.27t_my_handler_fields_being_re-injected.3F">this wiki page</a>, it is not recommended to define @Inject fields in a handler. <br/><br/>
<b>Inject the values in the @Execute methods</b>
*/
public class HelloWorldHandler {

	@Execute
	public void execute(@Named(IServiceConstants.ACTIVE_SHELL) Shell s) {
		String pathsScanned = "";
		// iterate over projects in workspace
		for (IProject project: ResourcesPlugin.getWorkspace().getRoot().getProjects()) {
			// fetch path to main pom.xml file of project
			final String pomXmlPath = project.getFile("pom.xml").getFullPath().toOSString();
			// run via API
			pathsScanned += pomXmlPath + ", ";
		};

		MessageDialog.openInformation(s, "Snyk", "Paths scanned: " + pathsScanned);
	}
}
