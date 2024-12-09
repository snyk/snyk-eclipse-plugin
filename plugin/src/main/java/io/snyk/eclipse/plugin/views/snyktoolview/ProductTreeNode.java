package io.snyk.eclipse.plugin.views.snyktoolview;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;

import org.eclipse.jface.viewers.TreeNode;

import io.snyk.eclipse.plugin.domain.ProductConstants;
import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.SnykIcons;

public class ProductTreeNode extends BaseTreeNode {

	private String product;
	private String errorMessage;
	private String prefEnablementKey;

	public ProductTreeNode(String value) {
		super(value);
		this.setProduct(value);

		switch (value) {
		case ProductConstants.DISPLAYED_OSS:
			setImageDescriptor(SnykIcons.OSS);
			prefEnablementKey = Preferences.ACTIVATE_SNYK_OPEN_SOURCE;
			break;
		case ProductConstants.DISPLAYED_IAC:
			setImageDescriptor(SnykIcons.IAC);
			prefEnablementKey = Preferences.ACTIVATE_SNYK_IAC;
			break;
		case ProductConstants.DISPLAYED_CODE_QUALITY:
			setImageDescriptor(SnykIcons.CODE);
			prefEnablementKey = Preferences.ACTIVATE_SNYK_CODE_QUALITY;
			break;
		case ProductConstants.DISPLAYED_CODE_SECURITY:
			setImageDescriptor(SnykIcons.CODE);
			prefEnablementKey = Preferences.ACTIVATE_SNYK_CODE_SECURITY;
			break;
		}
		
		
	}
	
	
	
	@Override
	public String getText() {
		if (!isEnabled()) {
			return super.getText() + " (disabled)";
		}
		return super.getText();
	}



	private boolean isEnabled() {
		Preferences pref = Preferences.getInstance();
		return pref.getBooleanPref(this.prefEnablementKey);
	}

	@Override
	public void setText(String text) {
		this.setValue(text);
	}

	public void removeInfoNodes() {
		var childs = getChildren();
		if (childs == null || childs.length == 0) {
			return;
		}

		var list = new ArrayList<TreeNode>(Arrays.asList(getChildren()));

		var newList = new HashSet<BaseTreeNode>();
		for (TreeNode node : list) {
			if (!(node instanceof InfoTreeNode) && node instanceof BaseTreeNode) {
				newList.add((BaseTreeNode) node);
			}
		}

		setChildren(newList.toArray(new BaseTreeNode[0]));
	}
	
	@Override
	public void setValue(Object value) {
		if (!(value instanceof String))
			throw new IllegalArgumentException("value of product node must be a string");
		if (!isEnabled()) {
			value = "(disabled)";
		}
		var cleanedValue = removePrefix(value.toString());

		// we don't want to override the product text
		if (!cleanedValue.isBlank()) {
			cleanedValue = product + " - " + cleanedValue;
		} else {
			cleanedValue = product;
		}
		super.setText(cleanedValue);
		super.setValue(cleanedValue);
	}

	@Override
	public void addChild(BaseTreeNode child) {
		if (!isEnabled()) return;
		if (child instanceof FileTreeNode) {
			var ftNode = (FileTreeNode) child;
			var crNode = (ContentRootNode) this.getParent();
			if (!ftNode.getPath().startsWith(crNode.getPath())) {
				throw new IllegalArgumentException(
						ftNode.getPath().toString() + " is not a sub path of " + crNode.getPath().toString());
			}
		}
		
		super.addChild(child);
	}

	private String removePrefix(String value) {
		var cleanedValue = value.toString();
		cleanedValue = cleanedValue.replace(product, "");
		return cleanedValue.replace(" - ", "");
	}

	@Override
	public void reset() {
		this.removeChildren();
		this.setValue(product);
		this.setErrorMessage(null);
	}

	public String getProduct() {
		return product;
	}

	public void setProduct(String product) {
		this.product = product;
	}

	public String getErrorMessage() {
		return errorMessage;
	}

	public void setErrorMessage(String errorMessage) {
		this.errorMessage = errorMessage;
	}
}
