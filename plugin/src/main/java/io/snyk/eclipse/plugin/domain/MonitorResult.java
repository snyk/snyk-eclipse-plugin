package io.snyk.eclipse.plugin.domain;

public class MonitorResult {
  boolean ok;
  String id;
  String uri;
  String path;
  String error;

  public MonitorResult() {
  }

  public static MonitorResult error(String errorMessage) {
    MonitorResult result = new MonitorResult();
    result.error = errorMessage;
    return result;
  }

  public boolean hasError() {
    return error != null && !error.isEmpty();
  }

  public boolean isOk() {
    return this.ok;
  }

  public String getId() {
    return this.id;
  }

  public String getUri() {
    return this.uri;
  }

  public String getPath() {
    return this.path;
  }

  public String getError() {
    return this.error;
  }

  public void setOk(boolean ok) {
    this.ok = ok;
  }

  public void setId(String id) {
    this.id = id;
  }

  public void setUri(String uri) {
    this.uri = uri;
  }

  public void setPath(String path) {
    this.path = path;
  }

  public void setError(String error) {
    this.error = error;
  }

  public boolean equals(final Object o) {
    if (o == this) return true;
    if (!(o instanceof MonitorResult)) return false;
    final MonitorResult other = (MonitorResult) o;
    if (!other.canEqual(this)) return false;
    if (this.isOk() != other.isOk()) return false;
    final Object this$id = this.getId();
    final Object other$id = other.getId();
    if (this$id == null ? other$id != null : !this$id.equals(other$id)) return false;
    final Object this$uri = this.getUri();
    final Object other$uri = other.getUri();
    if (this$uri == null ? other$uri != null : !this$uri.equals(other$uri)) return false;
    final Object this$path = this.getPath();
    final Object other$path = other.getPath();
    if (this$path == null ? other$path != null : !this$path.equals(other$path)) return false;
    final Object this$error = this.getError();
    final Object other$error = other.getError();
    return this$error == null ? other$error == null : this$error.equals(other$error);
  }

  protected boolean canEqual(final Object other) {
    return other instanceof MonitorResult;
  }

  public int hashCode() {
    final int PRIME = 59;
    int result = 1;
    result = result * PRIME + (this.isOk() ? 79 : 97);
    final Object $id = this.getId();
    result = result * PRIME + ($id == null ? 43 : $id.hashCode());
    final Object $uri = this.getUri();
    result = result * PRIME + ($uri == null ? 43 : $uri.hashCode());
    final Object $path = this.getPath();
    result = result * PRIME + ($path == null ? 43 : $path.hashCode());
    final Object $error = this.getError();
    result = result * PRIME + ($error == null ? 43 : $error.hashCode());
    return result;
  }

  public String toString() {
    return "MonitorResult(ok=" + this.isOk() + ", id=" + this.getId() + ", uri=" + this.getUri() + ", path=" + this.getPath() + ", error=" + this.getError() + ")";
  }
}
