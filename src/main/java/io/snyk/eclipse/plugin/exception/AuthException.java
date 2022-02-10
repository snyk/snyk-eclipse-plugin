package io.snyk.eclipse.plugin.exception;

public class AuthException extends Exception {
	
	private static final long serialVersionUID = -2440393279986418421L;

	public AuthException(String message) {
        super(message);
    }
	
	public AuthException(String message, Throwable cause) {
        super(message, cause);
    }
}
