package io.snyk.eclipse.plugin.views.snyktoolview;

import org.eclipse.jface.resource.ImageDescriptor;

import io.snyk.eclipse.plugin.Activator;
import io.snyk.eclipse.plugin.domain.ProductConstants;

public class ProductTreeNode extends BaseTreeNode {	
	public static final ImageDescriptor OSS = Activator.getImageDescriptor("/icons/oss.png");
	public static final ImageDescriptor CODE = Activator.getImageDescriptor("/icons/code.png");
	public static final ImageDescriptor IAC = Activator.getImageDescriptor("/icons/iac.png");
	
	private String product;

	public ProductTreeNode(Object value) {
		super(value);
		this.setProduct(value.toString());
		
		switch (value.toString()) {
		case ProductConstants.DISPLAYED_OSS:
			setImageDescriptor(OSS);
			break;
		case ProductConstants.DISPLAYED_IAC:
			setImageDescriptor(IAC);
			break;
		case ProductConstants.DISPLAYED_CODE_QUALITY:
			setImageDescriptor(CODE);
			break;
		case ProductConstants.DISPLAYED_CODE_SECURITY:
			setImageDescriptor(CODE);
			break;
		}
	}
	
	

	@Override
	public void setText(String text) {
		this.setValue(text);
	}



	@Override
	public void setValue(Object value) {
		if (!(value instanceof String)) throw new IllegalArgumentException("value of product node must be a string");
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
