package io.snyk.eclipse.plugin.views.provider;

import java.util.List;

import org.eclipse.jface.viewers.ITreeContentProvider;

import io.snyk.eclipse.plugin.views.DisplayModel;

public class TreeContentProvider implements ITreeContentProvider {

  @Override
  public boolean hasChildren(Object element) {
    List<DisplayModel> children = ((DisplayModel) element).children;
    return children != null && children.size() > 0;
  }

  @Override
  public Object getParent(Object element) {
    if (element == null) {
      return null;
    }

    return ((DisplayModel) element).parent;
  }

  @Override
  public Object[] getElements(Object inputElement) {
    return ((DisplayModel) inputElement).children.toArray();
  }

  @Override
  public Object[] getChildren(Object parentElement) {
    return ((DisplayModel) parentElement).children.toArray();
  }
}
