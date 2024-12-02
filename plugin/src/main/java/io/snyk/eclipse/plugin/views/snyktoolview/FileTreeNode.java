package io.snyk.eclipse.plugin.views.snyktoolview;

import java.nio.file.Path;
import java.nio.file.Paths;

import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.TreeNode;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.model.WorkbenchLabelProvider;

import io.snyk.eclipse.plugin.utils.SnykLogger;

public class FileTreeNode extends BaseTreeNode {
	private Path path;

	public FileTreeNode(String value) {
		super(value);
		this.setPath(Paths.get(value));
		this.setText(value);
	}

	@Override
	public ImageDescriptor getImageDescriptor() {
		ILabelProvider labelProvider = WorkbenchLabelProvider.getDecoratingWorkbenchLabelProvider();
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
		return getPath().toString();
	}

	public Path getPath() {
		return path;
	}

	public void setPath(Path path) {
		this.path = path.normalize();
	}

	@Override
	public void setParent(TreeNode parent) {
		if (!(parent instanceof ProductTreeNode))
			throw new IllegalArgumentException(
					parent.getClass().getName() + " cannot be a parent node to a FileTreeNode");
		var crNode = (ContentRootNode) parent.getParent();
		if (!this.getPath().startsWith(crNode.getPath()))
			throw new IllegalArgumentException(crNode.getPath() + " is not a sub path of " + this.getPath());
		super.setParent(parent);
	}

}
