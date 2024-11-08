package io.snyk.languageserver.protocolextension;

public class InvalidProductTypeException extends Exception {
    public InvalidProductTypeException(String message) {
        super(message);
    }
}
