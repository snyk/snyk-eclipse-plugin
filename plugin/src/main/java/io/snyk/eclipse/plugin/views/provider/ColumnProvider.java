package io.snyk.eclipse.plugin.views.provider;

import io.snyk.eclipse.plugin.views.DisplayModel;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.swt.graphics.Image;

import java.util.function.Function;

public class ColumnProvider extends ColumnLabelProvider {
  Function<DisplayModel, Image> vulnFuncImg;
  Function<DisplayModel, String> vulnFuncTxt;

  public ColumnProvider(Function<DisplayModel, Image> vulnFuncImg, Function<DisplayModel, String> vulnFuncTxt) {
    this.vulnFuncImg = vulnFuncImg;
    this.vulnFuncTxt = vulnFuncTxt;
  }

  @Override
  public String getText(Object element) {
    DisplayModel v = (DisplayModel) element;
    return vulnFuncTxt.apply(v);
  }

  @Override
  public Image getImage(Object element) {
    DisplayModel v = (DisplayModel) element;
    return vulnFuncImg.apply(v);
  }

}
