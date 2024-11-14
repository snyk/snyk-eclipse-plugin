package io.snyk.eclipse.plugin.views.snyktoolview;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import org.eclipse.jface.viewers.TreeNode;

public class BaseTreeNode extends TreeNode {

	public BaseTreeNode(Object value) {
		super(value);
	}

	public void setValue(Object value) {
		this.value = value;
	}

	public void addChild(BaseTreeNode child) {
		TreeNode[] children = getChildren();
		List<BaseTreeNode> list = new ArrayList<BaseTreeNode>();
		if (children != null) {
			var previousList = Arrays.asList(children)
					.stream()
				    .map(it -> (BaseTreeNode) it)
				    .collect(Collectors.toList());
			
			list = new ArrayList<BaseTreeNode>(previousList);
		}
		list.add(child);		
		this.setChildren(list.toArray(new BaseTreeNode[list.size()]));
	}
}
