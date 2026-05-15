package io.snyk.languageserver;

import java.util.Collections;
import java.util.EnumMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.function.Function;

import io.snyk.eclipse.plugin.preferences.AuthConstants;
import io.snyk.eclipse.plugin.preferences.Preferences;

/**
 * Single source of truth binding LS keys to Eclipse pref keys, with serializers for both
 * the outbound (IDE→LS) and inbound (LS→IDE) directions.
 *
 * ENTRIES is an ordered EnumMap covering all LsKey values used in outbound iteration.
 * BY_LS_KEY is a string-keyed map (including inbound-only entries) for inbound lookup.
 *
 * Inline special-cases in LsConfigurationUpdater (not in ENTRIES):
 * - RISK_SCORE_THRESHOLD: conditional int-or-null
 * - SEVERITY_FILTER_*: 4 keys sharing anySeverityChanged flag
 * - TRUSTED_FOLDERS: always-changed array, also set top-level for InitializationOptions
 * - ADDITIONAL_ENV: folder-scope only
 */
public final class LsSettingsRegistry {

    public static final class Entry {
        public final LsKey lsKey;
        /** null for hardcoded entries (no pref backing). */
        public final String prefKey;
        public final String outboundDefault;
        /** Converts pref string value → LS value (outbound). */
        public final Function<String, Object> outboundSerializer;
        /** Converts LS value (Object) → pref string (inbound). null for hardcoded entries. */
        public final Function<Object, String> inboundDeserializer;
        /** Always sent with changed=true — value treated as user-set regardless of tracking. */
        public final boolean alwaysChanged;
        /** Included when the fallback HTML settings form is active. */
        public final boolean useInFallbackForm;

        private Entry(LsKey lsKey, String prefKey, String outboundDefault,
                Function<String, Object> outboundSerializer,
                Function<Object, String> inboundDeserializer,
                boolean alwaysChanged, boolean useInFallbackForm) {
            this.lsKey = lsKey;
            this.prefKey = prefKey;
            this.outboundDefault = outboundDefault;
            this.outboundSerializer = outboundSerializer;
            this.inboundDeserializer = inboundDeserializer;
            this.alwaysChanged = alwaysChanged;
            this.useInFallbackForm = useInFallbackForm;
        }

        static Entry simple(LsKey lsKey, String prefKey, String outboundDefault) {
            return new Entry(lsKey, prefKey, outboundDefault, v -> v, LsSettingsRegistry::defaultToString, false, false);
        }

        static Entry fallback(LsKey lsKey, String prefKey, String outboundDefault) {
            return new Entry(lsKey, prefKey, outboundDefault, v -> v, LsSettingsRegistry::defaultToString, false, true);
        }

        /** Outbound sends Java Boolean (not string). LS GetBool handles both bool and "true"/"false". */
        static Entry bool(LsKey lsKey, String prefKey, boolean outboundDefault) {
            return new Entry(lsKey, prefKey, Boolean.toString(outboundDefault),
                    v -> Boolean.parseBoolean(v),
                    LsSettingsRegistry::defaultToString, false, false);
        }

        static Entry boolFallback(LsKey lsKey, String prefKey, boolean outboundDefault) {
            return new Entry(lsKey, prefKey, Boolean.toString(outboundDefault),
                    v -> Boolean.parseBoolean(v),
                    LsSettingsRegistry::defaultToString, false, true);
        }

        /** Value read from prefs normally, but always sent with changed=true. */
        static Entry alwaysChanged(LsKey lsKey, String prefKey, String outboundDefault) {
            return new Entry(lsKey, prefKey, outboundDefault, v -> v, LsSettingsRegistry::defaultToString, true, false);
        }

        /** Hardcoded value (no pref), always sent with changed=true. Matches VSCode alwaysChanged+hardcoded resolve. */
        static Entry fixed(LsKey lsKey, String fixedValue) {
            return new Entry(lsKey, null, fixedValue, v -> v, null, true, false);
        }
    }

    /**
     * Ordered outbound entries keyed by LsKey.
     * Iterated by LsConfigurationUpdater.buildConfigurationParam().
     * EnumMap preserves insertion order.
     */
    public static final Map<LsKey, Entry> ENTRIES;

    /**
     * String-keyed map for inbound lookup (SnykExtendedLanguageClient.persistGlobalSettings).
     * Includes inbound-only entries (severity filters, risk_score_threshold, trusted_folders)
     * not present in ENTRIES.
     */
    public static final Map<String, Entry> BY_LS_KEY;

    static {
        Map<LsKey, Entry> entries = new EnumMap<>(LsKey.class);

        entries.put(LsKey.ENDPOINT,               Entry.simple(LsKey.ENDPOINT, Preferences.ENDPOINT_KEY, ""));
        entries.put(LsKey.TOKEN,                  Entry.alwaysChanged(LsKey.TOKEN, Preferences.AUTH_TOKEN_KEY, ""));
        entries.put(LsKey.ORGANIZATION,           Entry.simple(LsKey.ORGANIZATION, Preferences.ORGANIZATION_KEY, ""));
        entries.put(LsKey.AUTHENTICATION_METHOD,  Entry.simple(LsKey.AUTHENTICATION_METHOD, Preferences.AUTHENTICATION_METHOD, AuthConstants.AUTH_OAUTH2));
        entries.put(LsKey.AUTOMATIC_AUTHENTICATION, Entry.fixed(LsKey.AUTOMATIC_AUTHENTICATION, "false"));
        entries.put(LsKey.ACTIVATE_SNYK_CODE,     Entry.bool(LsKey.ACTIVATE_SNYK_CODE, Preferences.ACTIVATE_SNYK_CODE_SECURITY, false));
        entries.put(LsKey.ACTIVATE_SNYK_OPEN_SOURCE, Entry.bool(LsKey.ACTIVATE_SNYK_OPEN_SOURCE, Preferences.ACTIVATE_SNYK_OPEN_SOURCE, true));
        entries.put(LsKey.ACTIVATE_SNYK_IAC,      Entry.bool(LsKey.ACTIVATE_SNYK_IAC, Preferences.ACTIVATE_SNYK_IAC, true));
        entries.put(LsKey.ACTIVATE_SNYK_SECRETS,  Entry.bool(LsKey.ACTIVATE_SNYK_SECRETS, Preferences.ACTIVATE_SNYK_SECRETS, false));
        // scan_automatic: LS expects boolean. "true" = automatic, "false" = manual.
        entries.put(LsKey.SCANNING_MODE, new Entry(LsKey.SCANNING_MODE, Preferences.SCANNING_MODE_AUTOMATIC, "false",
                v -> Boolean.parseBoolean(v),
                value -> String.valueOf("automatic".equals(String.valueOf(value))),
                false, false));
        entries.put(LsKey.ENABLE_DELTA_FINDINGS,  Entry.bool(LsKey.ENABLE_DELTA_FINDINGS, Preferences.ENABLE_DELTA, false));
        entries.put(LsKey.SEND_ERROR_REPORTS,     Entry.simple(LsKey.SEND_ERROR_REPORTS, Preferences.SEND_ERROR_REPORTS, ""));
        entries.put(LsKey.MANAGE_BINARIES_AUTOMATICALLY, Entry.boolFallback(LsKey.MANAGE_BINARIES_AUTOMATICALLY, Preferences.MANAGE_BINARIES_AUTOMATICALLY, true));
        entries.put(LsKey.CLI_PATH,               Entry.fallback(LsKey.CLI_PATH, Preferences.CLI_PATH, ""));
        entries.put(LsKey.CLI_BASE_DOWNLOAD_URL,  Entry.fallback(LsKey.CLI_BASE_DOWNLOAD_URL, Preferences.CLI_BASE_URL, "https://downloads.snyk.io"));
        entries.put(LsKey.INSECURE,               Entry.boolFallback(LsKey.INSECURE, Preferences.INSECURE_KEY, false));
        entries.put(LsKey.ADDITIONAL_PARAMS,      Entry.simple(LsKey.ADDITIONAL_PARAMS, Preferences.ADDITIONAL_PARAMETERS, ""));
        entries.put(LsKey.ENABLE_TRUSTED_FOLDERS_FEATURE, Entry.fixed(LsKey.ENABLE_TRUSTED_FOLDERS_FEATURE, Boolean.TRUE.toString()));
        entries.put(LsKey.ISSUE_VIEW_OPEN_ISSUES, Entry.bool(LsKey.ISSUE_VIEW_OPEN_ISSUES, Preferences.FILTER_IGNORES_SHOW_OPEN_ISSUES, true));
        entries.put(LsKey.ISSUE_VIEW_IGNORED_ISSUES, Entry.bool(LsKey.ISSUE_VIEW_IGNORED_ISSUES, Preferences.FILTER_IGNORES_SHOW_IGNORED_ISSUES, false));
        entries.put(LsKey.CLI_RELEASE_CHANNEL,    Entry.fallback(LsKey.CLI_RELEASE_CHANNEL, Preferences.RELEASE_CHANNEL, "stable"));

        ENTRIES = Collections.unmodifiableMap(entries);

        // Build BY_LS_KEY from ENTRIES plus inbound-only entries.
        Map<String, Entry> map = new LinkedHashMap<>();
        for (Entry e : ENTRIES.values()) {
            map.put(e.lsKey.key, e);
        }
        // Severity filters: inbound-only (outbound uses inline block in LsConfigurationUpdater).
        map.put(LsKey.SEVERITY_FILTER_CRITICAL.key, Entry.bool(LsKey.SEVERITY_FILTER_CRITICAL, Preferences.FILTER_SHOW_CRITICAL, true));
        map.put(LsKey.SEVERITY_FILTER_HIGH.key,     Entry.bool(LsKey.SEVERITY_FILTER_HIGH, Preferences.FILTER_SHOW_HIGH, true));
        map.put(LsKey.SEVERITY_FILTER_MEDIUM.key,   Entry.bool(LsKey.SEVERITY_FILTER_MEDIUM, Preferences.FILTER_SHOW_MEDIUM, true));
        map.put(LsKey.SEVERITY_FILTER_LOW.key,      Entry.bool(LsKey.SEVERITY_FILTER_LOW, Preferences.FILTER_SHOW_LOW, true));
        // risk_score_threshold: inbound int-or-null → pref string ("0" = no threshold)
        map.put(LsKey.RISK_SCORE_THRESHOLD.key, new Entry(LsKey.RISK_SCORE_THRESHOLD, Preferences.RISK_SCORE_THRESHOLD, "0",
                v -> v, value -> {
                    if (value == null) return "0";
                    if (value instanceof Number) {
                        long l = ((Number) value).longValue();
                        return l > 0 ? String.valueOf(l) : "0";
                    }
                    return String.valueOf(value);
                }, false, false));
        // trusted_folders: inbound String[] → pathSeparator-joined pref string
        map.put(LsKey.TRUSTED_FOLDERS.key, new Entry(LsKey.TRUSTED_FOLDERS, Preferences.TRUSTED_FOLDERS, "",
                v -> v, value -> {
                    if (value == null) return "";
                    if (value instanceof java.util.List) {
                        return ((java.util.List<?>) value).stream()
                                .map(String::valueOf)
                                .filter(s -> !s.isBlank())
                                .collect(java.util.stream.Collectors.joining(java.io.File.pathSeparator));
                    }
                    return String.valueOf(value);
                }, false, false));
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
