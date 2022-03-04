package io.snyk.eclipse.plugin.domain;

import java.util.List;

public class Patch {
    List<String> comments;
    String id;
    String modificationTime;
    List<String> urls;
    String version;

    public Patch() {
    }

    public List<String> getComments() {
        return this.comments;
    }

    public String getId() {
        return this.id;
    }

    public String getModificationTime() {
        return this.modificationTime;
    }

    public List<String> getUrls() {
        return this.urls;
    }

    public String getVersion() {
        return this.version;
    }

    public void setComments(List<String> comments) {
        this.comments = comments;
    }

    public void setId(String id) {
        this.id = id;
    }

    public void setModificationTime(String modificationTime) {
        this.modificationTime = modificationTime;
    }

    public void setUrls(List<String> urls) {
        this.urls = urls;
    }

    public void setVersion(String version) {
        this.version = version;
    }

    public boolean equals(final Object o) {
        if (o == this) return true;
        if (!(o instanceof Patch)) return false;
        final Patch other = (Patch) o;
        if (!other.canEqual(this)) return false;
        final Object this$comments = this.getComments();
        final Object other$comments = other.getComments();
        if (this$comments == null ? other$comments != null : !this$comments.equals(other$comments)) return false;
        final Object this$id = this.getId();
        final Object other$id = other.getId();
        if (this$id == null ? other$id != null : !this$id.equals(other$id)) return false;
        final Object this$modificationTime = this.getModificationTime();
        final Object other$modificationTime = other.getModificationTime();
        if (this$modificationTime == null ? other$modificationTime != null : !this$modificationTime.equals(other$modificationTime))
            return false;
        final Object this$urls = this.getUrls();
        final Object other$urls = other.getUrls();
        if (this$urls == null ? other$urls != null : !this$urls.equals(other$urls)) return false;
        final Object this$version = this.getVersion();
        final Object other$version = other.getVersion();
        return this$version == null ? other$version == null : this$version.equals(other$version);
    }

    protected boolean canEqual(final Object other) {
        return other instanceof Patch;
    }

    public int hashCode() {
        final int PRIME = 59;
        int result = 1;
        final Object $comments = this.getComments();
        result = result * PRIME + ($comments == null ? 43 : $comments.hashCode());
        final Object $id = this.getId();
        result = result * PRIME + ($id == null ? 43 : $id.hashCode());
        final Object $modificationTime = this.getModificationTime();
        result = result * PRIME + ($modificationTime == null ? 43 : $modificationTime.hashCode());
        final Object $urls = this.getUrls();
        result = result * PRIME + ($urls == null ? 43 : $urls.hashCode());
        final Object $version = this.getVersion();
        result = result * PRIME + ($version == null ? 43 : $version.hashCode());
        return result;
    }

    public String toString() {
        return "Patch(comments=" + this.getComments() + ", id=" + this.getId() + ", modificationTime=" + this.getModificationTime() + ", urls=" + this.getUrls() + ", version=" + this.getVersion() + ")";
    }
}
