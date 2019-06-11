package io.snyk.eclipse.plugin.views;

import java.util.function.Function;

import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.swt.graphics.Image;

import lombok.AllArgsConstructor;

@AllArgsConstructor
public class ColumnProvider extends ColumnLabelProvider{
	Function<DisplayModel, Image> vulnFuncImg;
	Function<DisplayModel, String> vulnFuncTxt;
	
	public String getText(Object element) {
		DisplayModel v = (DisplayModel) element;
		return vulnFuncTxt.apply(v);
	}
	
	public Image getImage(Object element) {
		DisplayModel v = (DisplayModel) element;
		return vulnFuncImg.apply(v);
	}
	
}
