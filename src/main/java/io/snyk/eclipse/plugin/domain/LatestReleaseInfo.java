package io.snyk.eclipse.plugin.domain;

import com.fasterxml.jackson.annotation.JsonProperty;

public class LatestReleaseInfo {
	Long id;
    String url;
    String name;
    
    @JsonProperty("tag_name")
    String tagName;

    public LatestReleaseInfo() {
    }

    public Long getId() {
        return this.id;
    }

    public String getUrl() {
        return this.url;
    }

    public String getName() {
        return this.name;
    }

    public String getTagName() {
        return this.tagName;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public void setUrl(String url) {
        this.url = url;
    }

    public void setName(String name) {
        this.name = name;
    }

    @JsonProperty("tag_name")
    public void setTagName(String tagName) {
        this.tagName = tagName;
    }

    public boolean equals(final Object o) {
        if (o == this) return true;
        if (!(o instanceof LatestReleaseInfo)) return false;
        final LatestReleaseInfo other = (LatestReleaseInfo) o;
        if (!other.canEqual((Object) this)) return false;
        final Object this$id = this.getId();
        final Object other$id = other.getId();
        if (this$id == null ? other$id != null : !this$id.equals(other$id)) return false;
        final Object this$url = this.getUrl();
        final Object other$url = other.getUrl();
        if (this$url == null ? other$url != null : !this$url.equals(other$url)) return false;
        final Object this$name = this.getName();
        final Object other$name = other.getName();
        if (this$name == null ? other$name != null : !this$name.equals(other$name)) return false;
        final Object this$tagName = this.getTagName();
        final Object other$tagName = other.getTagName();
        if (this$tagName == null ? other$tagName != null : !this$tagName.equals(other$tagName)) return false;
        return true;
    }

    protected boolean canEqual(final Object other) {
        return other instanceof LatestReleaseInfo;
    }

    public int hashCode() {
        final int PRIME = 59;
        int result = 1;
        final Object $id = this.getId();
        result = result * PRIME + ($id == null ? 43 : $id.hashCode());
        final Object $url = this.getUrl();
        result = result * PRIME + ($url == null ? 43 : $url.hashCode());
        final Object $name = this.getName();
        result = result * PRIME + ($name == null ? 43 : $name.hashCode());
        final Object $tagName = this.getTagName();
        result = result * PRIME + ($tagName == null ? 43 : $tagName.hashCode());
        return result;
    }

    public String toString() {
        return "LatestReleaseInfo(id=" + this.getId() + ", url=" + this.getUrl() + ", name=" + this.getName() + ", tagName=" + this.getTagName() + ")";
    }
}
