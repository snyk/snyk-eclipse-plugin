package io.snyk.languageserver;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

import io.snyk.eclipse.plugin.preferences.AuthConstants;
import io.snyk.eclipse.plugin.preferences.Preferences;

/**
 * Single source of truth binding LS keys to Eclipse pref keys, with serializers for both
 * the outbound (IDE→LS) and inbound (LS→IDE) directions.
 *
 * ENTRIES is the ordered list used for outbound iteration.
 * BY_LS_KEY also includes inbound-only entries (severity filters) not in ENTRIES,
 * because their outbound serialization requires cross-key logic (anySeverityChanged).
 *
 * Excluded from ENTRIES and handled inline in LsConfigurationUpdater:
 * - risk_score_threshold: conditional int-or-null
 * - severity_filter_*: 4 individual keys with shared anySeverityChanged flag
 * - trusted_folders: always-changed array, also set top-level for InitializationOptions
 * - additional_environment: folder-scope only (not a global setting)
 */
public final class LsSettingsRegistry {

    public static final class Entry {
        public final String lsKey;
        public final String prefKey;
        public final String outboundDefault;
        /** Converts pref string value → LS value (outbound). */
        public final Function<String, Object> outboundSerializer;
        /** Converts LS value (Object) → pref string (inbound). */
        public final Function<Object, String> inboundDeserializer;
        /** If true the entry is always sent with the given outboundDefault regardless of prefs. */
        public final boolean alwaysFixed;

        private Entry(String lsKey, String prefKey, String outboundDefault,
                Function<String, Object> outboundSerializer,
                Function<Object, String> inboundDeserializer,
                boolean alwaysFixed) {
            this.lsKey = lsKey;
            this.prefKey = prefKey;
            this.outboundDefault = outboundDefault;
            this.outboundSerializer = outboundSerializer;
            this.inboundDeserializer = inboundDeserializer;
            this.alwaysFixed = alwaysFixed;
        }

        static Entry simple(String lsKey, String prefKey, String outboundDefault) {
            return new Entry(lsKey, prefKey, outboundDefault, v -> v, LsSettingsRegistry::defaultToString, false);
        }

        /** Outbound sends Java Boolean (not string). LS GetBool handles both bool and "true"/"false". */
        static Entry bool(String lsKey, String prefKey, boolean outboundDefault) {
            return new Entry(lsKey, prefKey, Boolean.toString(outboundDefault),
                    v -> Boolean.parseBoolean(v),
                    LsSettingsRegistry::defaultToString, false);
        }

        static Entry fixed(String lsKey, String fixedValue) {
            return new Entry(lsKey, null, fixedValue, v -> v, null, true);
        }
    }

    /** Ordered outbound entries. Iterated by LsConfigurationUpdater.buildConfigurationParam(). */
    public static final List<Entry> ENTRIES;

    /**
     * All entries including inbound-only ones (severity filters).
     * Used by SnykExtendedLanguageClient.persistGlobalSettings().
     */
    public static final Map<String, Entry> BY_LS_KEY;

    static {
        List<Entry> entries = new ArrayList<>();

        entries.add(Entry.simple(LsSettingsKeys.ENDPOINT, Preferences.ENDPOINT_KEY, ""));
        entries.add(new Entry(LsSettingsKeys.TOKEN, Preferences.AUTH_TOKEN_KEY, "",
                v -> v, LsSettingsRegistry::defaultToString, false));
        entries.add(Entry.simple(LsSettingsKeys.ORGANIZATION, Preferences.ORGANIZATION_KEY, ""));
        entries.add(Entry.simple(LsSettingsKeys.AUTHENTICATION_METHOD, Preferences.AUTHENTICATION_METHOD,
                AuthConstants.AUTH_OAUTH2));
        entries.add(Entry.fixed(LsSettingsKeys.AUTOMATIC_AUTHENTICATION, "false"));
        entries.add(Entry.bool(LsSettingsKeys.ACTIVATE_SNYK_CODE, Preferences.ACTIVATE_SNYK_CODE_SECURITY, false));
        entries.add(Entry.bool(LsSettingsKeys.ACTIVATE_SNYK_OPEN_SOURCE, Preferences.ACTIVATE_SNYK_OPEN_SOURCE, true));
        entries.add(Entry.bool(LsSettingsKeys.ACTIVATE_SNYK_IAC, Preferences.ACTIVATE_SNYK_IAC, true));
        entries.add(Entry.bool(LsSettingsKeys.ACTIVATE_SNYK_SECRETS, Preferences.ACTIVATE_SNYK_SECRETS, false));
        // scan_automatic: LS expects boolean. "true" = automatic, "false" = manual.
        entries.add(new Entry(LsSettingsKeys.SCANNING_MODE, Preferences.SCANNING_MODE_AUTOMATIC, "false",
                v -> Boolean.parseBoolean(v),
                value -> String.valueOf("automatic".equals(String.valueOf(value))),
                false));
        entries.add(Entry.bool(LsSettingsKeys.ENABLE_DELTA_FINDINGS, Preferences.ENABLE_DELTA, false));
        entries.add(Entry.simple(LsSettingsKeys.SEND_ERROR_REPORTS, Preferences.SEND_ERROR_REPORTS, ""));
        entries.add(Entry.bool(LsSettingsKeys.MANAGE_BINARIES_AUTOMATICALLY,
                Preferences.MANAGE_BINARIES_AUTOMATICALLY, true));
        entries.add(Entry.simple(LsSettingsKeys.CLI_PATH, Preferences.CLI_PATH, ""));
        entries.add(Entry.simple(LsSettingsKeys.CLI_BASE_DOWNLOAD_URL, Preferences.CLI_BASE_URL,
                "https://downloads.snyk.io"));
        entries.add(Entry.bool(LsSettingsKeys.INSECURE, Preferences.INSECURE_KEY, false));
        entries.add(Entry.simple(LsSettingsKeys.ADDITIONAL_PARAMS, Preferences.ADDITIONAL_PARAMETERS, ""));
        entries.add(Entry.fixed(LsSettingsKeys.ENABLE_TRUSTED_FOLDERS_FEATURE, Boolean.TRUE.toString()));
        entries.add(Entry.bool(LsSettingsKeys.ISSUE_VIEW_OPEN_ISSUES,
                Preferences.FILTER_IGNORES_SHOW_OPEN_ISSUES, true));
        entries.add(Entry.bool(LsSettingsKeys.ISSUE_VIEW_IGNORED_ISSUES,
                Preferences.FILTER_IGNORES_SHOW_IGNORED_ISSUES, false));
        entries.add(Entry.simple(LsSettingsKeys.CLI_RELEASE_CHANNEL, Preferences.RELEASE_CHANNEL, "stable"));

        ENTRIES = Collections.unmodifiableList(entries);

        // Build BY_LS_KEY from ENTRIES plus inbound-only severity entries.
        Map<String, Entry> map = new LinkedHashMap<>();
        for (Entry e : ENTRIES) {
            map.put(e.lsKey, e);
        }
        // Severity filters: inbound-only in this map (outbound uses inline block in LsConfigurationUpdater).
        map.put(LsSettingsKeys.SEVERITY_FILTER_CRITICAL,
                Entry.bool(LsSettingsKeys.SEVERITY_FILTER_CRITICAL, Preferences.FILTER_SHOW_CRITICAL, true));
        map.put(LsSettingsKeys.SEVERITY_FILTER_HIGH,
                Entry.bool(LsSettingsKeys.SEVERITY_FILTER_HIGH, Preferences.FILTER_SHOW_HIGH, true));
        map.put(LsSettingsKeys.SEVERITY_FILTER_MEDIUM,
                Entry.bool(LsSettingsKeys.SEVERITY_FILTER_MEDIUM, Preferences.FILTER_SHOW_MEDIUM, true));
        map.put(LsSettingsKeys.SEVERITY_FILTER_LOW,
                Entry.bool(LsSettingsKeys.SEVERITY_FILTER_LOW, Preferences.FILTER_SHOW_LOW, true));
        BY_LS_KEY = Collections.unmodifiableMap(map);
    }

    private LsSettingsRegistry() {
        throw new UnsupportedOperationException();
    }

    private static String defaultToString(Object value) {
        if (value instanceof Number) {
            double d = ((Number) value).doubleValue();
            if (d == Math.floor(d) && !Double.isInfinite(d)) {
                return String.valueOf((long) d);
            }
        }
        return String.valueOf(value);
    }
}
