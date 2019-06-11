package io.snyk.eclipse.plugin.views;

import java.util.function.Function;

import org.eclipse.jface.viewers.ColumnLabelProvider;

public class ColumnTextProvider extends ColumnLabelProvider{
	
	Function<DisplayModel, String> vulnFunc;
	
	public ColumnTextProvider(Function<DisplayModel, String> vulnFunc) {
		this.vulnFunc = vulnFunc;
	}
	
	public String getText(Object element) {
		DisplayModel v = (DisplayModel) element;
		return vulnFunc.apply(v);
	}
}
