package io.snyk.languageserver.protocolextension.messageObjects.scanResults;

public class Range {
    public Range() {

    }

    private LineRange start;
    private LineRange end;

    public LineRange getStart() {
        return start;
    }
    public void setStart(LineRange start) {
        this.start = start;
    }
    public LineRange getEnd() {
        return end;
    }
    public void setEnd(LineRange end) {
        this.end = end;
    }
}
