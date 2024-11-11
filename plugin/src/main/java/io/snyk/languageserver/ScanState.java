package io.snyk.languageserver;

import java.util.concurrent.ConcurrentHashMap;

public class ScanState {
	private ScanState() {
		
	}

	private final ConcurrentHashMap<ScanInProgressKey, Boolean> scanInProgress = new ConcurrentHashMap<>();
	
    private static ScanState instance = new ScanState();    
	public static ScanState getInstance() {
		if (instance == null) {
			synchronized (ScanState.class) {
				if (instance == null) {
					instance = new ScanState();
				}
			}
		}
		return instance;
	}

	
	public ConcurrentHashMap<ScanInProgressKey, Boolean> getScanInProgress() {
		return scanInProgress;
	}
}
