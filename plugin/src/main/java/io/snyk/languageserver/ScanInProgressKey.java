package io.snyk.languageserver;

import java.util.Objects;

public class ScanInProgressKey {
	public ScanInProgressKey(String folderPath, String product) {
		this.folderPath = folderPath;
		this.product = product;
	}
	String folderPath;
	String product;
	
	public String getFolderPath() {
		return folderPath;
	}
	public void setFolderPath(String folderPath) {
		this.folderPath = folderPath;
	}
	public String getProduct() {
		return product;
	}
	public void setProduct(String product) {
		this.product = product;
	}
	
	@Override
	public int hashCode() {
		return Objects.hash(folderPath, product);
	}
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		ScanInProgressKey other = (ScanInProgressKey) obj;
		return Objects.equals(folderPath, other.folderPath) && Objects.equals(product, other.product);
	}	
}