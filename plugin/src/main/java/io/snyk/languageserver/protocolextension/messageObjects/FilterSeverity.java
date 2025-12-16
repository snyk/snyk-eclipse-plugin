package io.snyk.languageserver.protocolextension.messageObjects;

public class FilterSeverity {
	private final boolean critical;
	private final boolean high;
	private final boolean medium;
	private final boolean low;

	public FilterSeverity(boolean critical, boolean high, boolean medium, boolean low) {
		this.critical = critical;
		this.high = high;
		this.medium = medium;
		this.low = low;
	}

	public boolean isCritical() {
		return critical;
	}

	public boolean isHigh() {
		return high;
	}

	public boolean isMedium() {
		return medium;
	}

	public boolean isLow() {
		return low;
	}
}
