package io.snyk.languageserver.protocolextension;

public class InvalidProductTypeException extends RuntimeException {
    public InvalidProductTypeException(String message) {
        super(message);
    }
}
