package io.snyk.eclipse.plugin.runner;

public class ProcessResult {
    private final String content;
    private final String error;


    public ProcessResult(String content, String error) {
        this.content = content;
        this.error = error;
    }

    public static ProcessResult error(String error) {
        return new ProcessResult("", error);
    }

    public boolean hasError() {
        return error != null && !error.isEmpty();
    }

    public boolean hasContent() {
        return content != null && !error.isEmpty();
    }

    public boolean hasContentError() {
        return content != null && content.contains("\"error\":");
    }

    public boolean hasErrorOrContentError() {
        return hasError() || hasContentError();
    }

    public String getErrorOrContent() {
        if (error == null) return content;
        return error;
    }


    public String getContent() {
        return this.content;
    }

    public String getError() {
        return this.error;
    }

    public String toString() {
        return "ProcessResult(content=" + this.getContent() + ", error=" + this.getError() + ")";
    }
}
