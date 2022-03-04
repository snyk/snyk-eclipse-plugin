package io.snyk.eclipse.plugin.exception;

public class NotSupportedException extends Exception {

    private static final long serialVersionUID = -2440393279986418616L;

    public NotSupportedException(String message) {
        super(message);
    }

    public NotSupportedException(String message, Throwable cause) {
        super(message, cause);
    }
}
