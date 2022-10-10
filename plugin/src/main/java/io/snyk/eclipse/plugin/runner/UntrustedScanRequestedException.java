package io.snyk.eclipse.plugin.runner;

public class UntrustedScanRequestedException extends RuntimeException {

  private static final long serialVersionUID = 4849361078384083852L;

  public UntrustedScanRequestedException() {
  }

  public UntrustedScanRequestedException(String message) {
    super(message);
  }

  public UntrustedScanRequestedException(Throwable cause) {
    super(cause);
  }

  public UntrustedScanRequestedException(String message, Throwable cause) {
    super(message, cause);
  }

  public UntrustedScanRequestedException(String message, Throwable cause, boolean enableSuppression,
      boolean writableStackTrace) {
    super(message, cause, enableSuppression, writableStackTrace);
  }

}
