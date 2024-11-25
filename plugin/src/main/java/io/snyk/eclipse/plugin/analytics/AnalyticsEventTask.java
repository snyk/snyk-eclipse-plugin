package io.snyk.eclipse.plugin.analytics;

import java.time.Instant;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import io.snyk.eclipse.plugin.properties.preferences.Preferences;

public class AnalyticsEventTask implements AbstractTask {
    private final String interactionType;
    private final List<String> category;
    private final String status;
    private final String targetId;
    private final long timestampMs;
    private final long durationMs;
    private final Map<String, Object> results;
    private final List<Object> errors;
    private final Map<String, Object> extension;

    public AnalyticsEventTask(String interactionType, List<String> category, String status, String targetId, long timestampMs, long durationMs, Map<String, Object> results, List<Object> errors, Map<String, Object> extension) {
        this.interactionType = interactionType;
        this.category = category;
        this.status = status != null ? status : "success";
        this.targetId = targetId != null ? targetId : "pkg:filesystem/scrubbed";
        this.timestampMs = timestampMs != 0 ? timestampMs : Instant.now().toEpochMilli() ;
        this.durationMs = durationMs;
        this.results = results != null ? results : new HashMap<>();
        this.errors = errors != null ? errors : new ArrayList<>();
        this.extension = extension != null ? extension : new HashMap<>();
        this.extension.put("device_id", Preferences.getInstance().getPref(Preferences.DEVICE_ID));
    }

    public AnalyticsEventTask(String interactionType, List<String> category) {
    	this(interactionType, category, null, null, 0, 0, null, null, null);
	}

	public String getInteractionType() {
        return interactionType;
    }

    public List<String> getCategory() {
        return category;
    }

    public String getStatus() {
        return status;
    }

    public String getTargetId() {
        return targetId;
    }

    public long getTimestampMs() {
        return timestampMs;
    }

    public long getDurationMs() {
        return durationMs;
    }

    public Map<String, Object> getResults() {
        return results;
    }

    public List<Object> getErrors() {
        return errors;
    }

    public Map<String, Object> getExtension() {
        return extension;
    }
}
