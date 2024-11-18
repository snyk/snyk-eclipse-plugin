package io.snyk.eclipse.plugin.views.snyktoolview;

import io.snyk.eclipse.plugin.domain.ProductConstants;
import io.snyk.eclipse.plugin.utils.SnykIcons;

public class ProductTreeNode extends BaseTreeNode {

	private String product;

	public ProductTreeNode(Object value) {
		super(value);
		this.setProduct(value.toString());

		switch (value.toString()) {
		case ProductConstants.DISPLAYED_OSS:
			setImageDescriptor(SnykIcons.OSS);
			break;
		case ProductConstants.DISPLAYED_IAC:
			setImageDescriptor(SnykIcons.IAC);
			break;
		case ProductConstants.DISPLAYED_CODE_QUALITY:
			setImageDescriptor(SnykIcons.CODE);
			break;
		case ProductConstants.DISPLAYED_CODE_SECURITY:
			setImageDescriptor(SnykIcons.CODE);
			break;
		}
	}

	@Override
	public void setText(String text) {
		this.setValue(text);
	}

	@Override
	public void setValue(Object value) {
		if (!(value instanceof String))
			throw new IllegalArgumentException("value of product node must be a string");
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

	private String removePrefix(String value) {
		var cleanedValue = value.toString();
		cleanedValue = cleanedValue.replace(product, "");
		return cleanedValue.replace(" - ", "");
	}

	@Override
	public void reset() {
		this.removeChildren();
		this.setValue(product);
	}

	public String getProduct() {
		return product;
	}

	public void setProduct(String product) {
		this.product = product;
	}
}
