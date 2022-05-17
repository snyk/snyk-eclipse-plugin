package io.snyk.languageserver.download;

public class ChecksumVerificationException extends RuntimeException {
  public ChecksumVerificationException(String s) {
    super(s);
  }

  public ChecksumVerificationException(Throwable cause) {
    super(cause);
  }
}
