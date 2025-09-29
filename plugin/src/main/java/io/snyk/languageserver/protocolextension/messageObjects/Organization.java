package io.snyk.languageserver.protocolextension.messageObjects;

import java.util.Objects;

public class Organization {
	private String slug;
	private String id;
	private String name;
	private boolean defaultOrg;
	private boolean preferredByAlgorithm;
	
	public String getSlug() {
		return slug;
	}
	public void setSlug(String slug) {
		this.slug = slug;
	}
	public String getId() {
		return id;
	}
	public void setId(String id) {
		this.id = id;
	}
	public boolean isDefaultOrg() {
		return defaultOrg;
	}
	public void setDefaultOrg(boolean defaultOrg) {
		this.defaultOrg = defaultOrg;
	}
	public boolean isPreferredByAlgorithm() {
		return preferredByAlgorithm;
	}
	public void setPreferredByAlgorithm(boolean preferredByAlgorithm) {
		this.preferredByAlgorithm = preferredByAlgorithm;
	}
	@Override
	public int hashCode() {
		return Objects.hash(defaultOrg, id, preferredByAlgorithm, slug);
	}
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		Organization other = (Organization) obj;
		return defaultOrg == other.defaultOrg && Objects.equals(id, other.id)
				&& preferredByAlgorithm == other.preferredByAlgorithm && Objects.equals(slug, other.slug);
	}
	@Override
	public String toString() {
		return "Organization [slug=" + slug + ", id=" + id + ", defaultOrg=" + defaultOrg + ", preferredByAlgorithm="
				+ preferredByAlgorithm + "]";
	}
	public String getName() {
		return name;
	}
	public void setName(String name) {
		this.name = name;
	}

}
