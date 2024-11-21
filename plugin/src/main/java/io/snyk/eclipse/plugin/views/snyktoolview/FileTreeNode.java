package io.snyk.eclipse.plugin.views.snyktoolview;

import java.nio.file.Path;
import java.nio.file.Paths;

import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.model.WorkbenchLabelProvider;

public class FileTreeNode extends BaseTreeNode {
	private Path path;

	public FileTreeNode(String value) {
		super(value);
		this.setPath(Paths.get(value));
	}

	@Override
	public ImageDescriptor getImageDescriptor() {
		// TODO: this does not display the file icon. Why?
		WorkbenchLabelProvider labelProvider = new WorkbenchLabelProvider();
		try {
			var files = ResourcesPlugin.getWorkspace().getRoot().findFilesForLocationURI(getPath().toUri());
			var object = files[0];
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

	public Path getPath() {
		return path;
	}

	public void setPath(Path path) {
		this.path = path;
	}
	
	
}
