package io.snyk.eclipse.plugin.domain;

public class ContentError {
    private boolean ok;
    private String error;
    private String path;

    public ContentError() {
    }

    public boolean isOk() {
        return this.ok;
    }

    public String getError() {
        return this.error;
    }

    public String getPath() {
        return this.path;
    }

    public void setOk(boolean ok) {
        this.ok = ok;
    }

    public void setError(String error) {
        this.error = error;
    }

    public void setPath(String path) {
        this.path = path;
    }

    public boolean equals(final Object o) {
        if (o == this) return true;
        if (!(o instanceof ContentError)) return false;
        final ContentError other = (ContentError) o;
        if (!other.canEqual(this)) return false;
        if (this.isOk() != other.isOk()) return false;
        final Object this$error = this.getError();
        final Object other$error = other.getError();
        if (this$error == null ? other$error != null : !this$error.equals(other$error)) return false;
        final Object this$path = this.getPath();
        final Object other$path = other.getPath();
        return this$path == null ? other$path == null : this$path.equals(other$path);
    }

    protected boolean canEqual(final Object other) {
        return other instanceof ContentError;
    }

    public int hashCode() {
        final int PRIME = 59;
        int result = 1;
        result = result * PRIME + (this.isOk() ? 79 : 97);
        final Object $error = this.getError();
        result = result * PRIME + ($error == null ? 43 : $error.hashCode());
        final Object $path = this.getPath();
        result = result * PRIME + ($path == null ? 43 : $path.hashCode());
        return result;
    }

    public String toString() {
        return "ContentError(ok=" + this.isOk() + ", error=" + this.getError() + ", path=" + this.getPath() + ")";
    }
}
