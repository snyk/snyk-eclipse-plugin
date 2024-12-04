package io.snyk.languageserver;

import java.util.concurrent.ConcurrentHashMap;

public class ScanState {
	private ScanState() {
		
	}

	private final ConcurrentHashMap<ScanInProgressKey, Boolean> scanInProgress = new ConcurrentHashMap<>();
	
    private static ScanState instance = new ScanState();    
	public static ScanState getInstance() {
		synchronized (ScanState.class) {
			if (instance == null) {
				if (instance == null) {
					instance = new ScanState();
				}
			}
		}
		return instance;
	}
	
    // ----- Methods to Manage Scan States -----

    /**
     * Checks if a scan is in progress for the given key.
     *
     * @param key The ScanInProgressKey identifying the scan
     * @return true if the scan is in progress, false otherwise
     */
    public boolean isScanInProgress(ScanInProgressKey key) {
        return scanInProgress.getOrDefault(key, false);
    }

    /**
     * Sets the scan state for the given key.
     *
     * @param key        The ScanInProgressKey identifying the scan
     * @param inProgress true if the scan is in progress, false otherwise
     */
    public void setScanInProgress(ScanInProgressKey key, boolean inProgress) {
        scanInProgress.put(key, inProgress);
    }

    /**
     * Removes the scan state for the given key.
     *
     * @param key The ScanInProgressKey identifying the scan
     */
    public void removeScanState(ScanInProgressKey key) {
        scanInProgress.remove(key);
    }

    /**
     * Clears all scan states.
     */
    public void clearAllScanStates() {
        scanInProgress.clear();
    }
}
