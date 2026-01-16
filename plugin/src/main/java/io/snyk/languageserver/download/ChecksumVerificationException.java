package io.snyk.languageserver.download;

public class ChecksumVerificationException extends Exception {
	private static final long serialVersionUID = 7210924932018107815L;

	public ChecksumVerificationException(String s) {
		super(s);
	}

	public ChecksumVerificationException(Throwable cause) {
		super(cause);
	}
}
