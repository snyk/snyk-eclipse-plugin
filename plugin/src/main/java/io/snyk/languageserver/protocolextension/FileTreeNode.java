package io.snyk.languageserver.protocolextension;

import java.nio.file.Path;
import java.nio.file.Paths;

import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.model.WorkbenchLabelProvider;

import io.snyk.eclipse.plugin.views.snyktoolview.BaseTreeNode;

public class FileTreeNode extends BaseTreeNode {
	private Path path;

	public FileTreeNode(String value) {
		super(value);
		this.path = Paths.get(value);
	}

	@Override
	public ImageDescriptor getImageDescriptor() {
		WorkbenchLabelProvider labelProvider = new WorkbenchLabelProvider();
		try {
			var object = ResourcesPlugin.getWorkspace().getRoot().findMember(this.path.toAbsolutePath().toString(), false);
			if (object == null) {
				return null;
			}
			Image image = labelProvider.getImage(object);
			if (image == null)
				return null;
			
			return ImageDescriptor.createFromImage(image);
		} finally {
			labelProvider.dispose();
		}
	}

	@Override
	public String getText() {
		// TODO Auto-generated method stub
		return super.getText();
	}
	
	
}
