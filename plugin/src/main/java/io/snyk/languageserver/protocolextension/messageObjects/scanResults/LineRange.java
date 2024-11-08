package io.snyk.languageserver.protocolextension.messageObjects.scanResults;

public class LineRange {
	public LineRange() {
		
	}
	
    private int line;
    private int character;

    public int getLine() { return line; }
    public void setLine(int line) { this.line = line; }

    public int getCharacter() { return character; }
    public void setCharacter(int character) { this.character = character; }
}