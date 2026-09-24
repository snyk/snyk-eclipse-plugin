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
        public final boolean isAlwaysChanged;
        public final boolean encrypted;
        /** Included when the fallback HTML settings form is active. */
        public final boolean useInFallbackForm;
        /**
         * Additional pref keys whose changed-state ORed with prefKey's to compute the changed flag.
         * Used for severity filters that share a changed flag across 4 keys.
         */
        public final String[] additionalChangedPrefKeys;
        /**
         * Converts a JSON form node (from __saveIdeConfig__ payload) → pref string.
         * Used by HTMLSettingsPreferencePage for form-specific deserialization.
         * null = use the standard applyFormValue path (raw nodeToString).
         */
        public final Function<com.fasterxml.jackson.databind.JsonNode, String> formDeserializer;

        private Entry(LsKey lsKey, String prefKey, String outboundDefault,
                Function<String, Object> outboundSerializer,
                Function<Object, String> inboundDeserializer,
                boolean alwaysChanged, boolean useInFallbackForm,
                String[] additionalChangedPrefKeys,
                Function<com.fasterxml.jackson.databind.JsonNode, String> formDeserializer,
                boolean encrypted) {
            this.lsKey = lsKey;
            this.prefKey = prefKey;
            this.outboundDefault = outboundDefault;
            this.outboundSerializer = outboundSerializer;
            this.inboundDeserializer = inboundDeserializer;
            this.isAlwaysChanged = alwaysChanged;
            this.encrypted = encrypted;
            this.useInFallbackForm = useInFallbackForm;
            this.additionalChangedPrefKeys = additionalChangedPrefKeys;
            this.formDeserializer = formDeserializer;
        }

        static Entry simple(LsKey lsKey, String prefKey, String outboundDefault) {
            return new Entry(lsKey, prefKey, outboundDefault, v -> v, LsSettingsRegistry::defaultToString, false, false, new String[0], null, false);
        }

        static Entry fallback(LsKey lsKey, String prefKey, String outboundDefault) {
            return new Entry(lsKey, prefKey, outboundDefault, v -> v, LsSettingsRegistry::defaultToString, false, true, new String[0], null, false);
        }

        /** Outbound sends Java Boolean (not string). LS GetBool handles both bool and "true"/"false". */
        static Entry bool(LsKey lsKey, String prefKey, boolean outboundDefault) {
            return new Entry(lsKey, prefKey, Boolean.toString(outboundDefault),
                    v -> Boolean.parseBoolean(v),
                    LsSettingsRegistry::defaultToString, false, false, new String[0], null, false);
        }

        static Entry boolFallback(LsKey lsKey, String prefKey, boolean outboundDefault) {
            return new Entry(lsKey, prefKey, Boolean.toString(outboundDefault),
                    v -> Boolean.parseBoolean(v),
                    LsSettingsRegistry::defaultToString, false, true, new String[0], null, false);
        }

        /** Value read from prefs normally, but always sent with changed=true. */
        static Entry alwaysChanged(LsKey lsKey, String prefKey, String outboundDefault) {
            return new Entry(lsKey, prefKey, outboundDefault, v -> v, LsSettingsRegistry::defaultToString, true, false, new String[0], null, false);
        }

        /** Hardcoded value (no pref), always sent with changed=true. Matches VSCode alwaysChanged+hardcoded resolve. */
        static Entry fixed(LsKey lsKey, String fixedValue) {
            return new Entry(lsKey, null, fixedValue, v -> v, null, true, false, new String[0], null, false);
        }

        static Entry encryptedAlwaysChanged(LsKey lsKey, String prefKey, String outboundDefault) {
            return new Entry(lsKey, prefKey, outboundDefault, v -> v, LsSettingsRegistry::defaultToString, true, false, new String[0], null, true);
        }
    }

    private static final String TRUE = "true";

    /**
     * Ordered outbound entries keyed by LsKey.
     * Iterated by LsConfigurationUpdater.buildConfigurationParam().
     * EnumMap preserves LsKey declaration order; reordering LsKey members changes the outbound config sequence.
     */
    public static final Map<LsKey, Entry> ENTRIES;

    /**
     * String-keyed map for inbound lookup (SnykExtendedLanguageClient.persistGlobalSettings).
     * Auto-populated from ENTRIES; all entries are included.
     */
    public static final Map<String, Entry> BY_LS_KEY;

    static {
        Map<LsKey, Entry> entries = new EnumMap<>(LsKey.class);

        entries.put(LsKey.ENDPOINT,               Entry.simple(LsKey.ENDPOINT, Preferences.ENDPOINT_KEY, Preferences.DEFAULT_ENDPOINT));
        entries.put(LsKey.TOKEN,                  Entry.encryptedAlwaysChanged(LsKey.TOKEN, Preferences.AUTH_TOKEN_KEY, ""));
        entries.put(LsKey.ORGANIZATION,           Entry.simple(LsKey.ORGANIZATION, Preferences.ORGANIZATION_KEY, ""));
        entries.put(LsKey.AUTHENTICATION_METHOD,  Entry.simple(LsKey.AUTHENTICATION_METHOD, Preferences.AUTHENTICATION_METHOD, AuthConstants.AUTH_OAUTH2));
        entries.put(LsKey.AUTOMATIC_AUTHENTICATION, Entry.fixed(LsKey.AUTOMATIC_AUTHENTICATION, "false"));
        entries.put(LsKey.ACTIVATE_SNYK_CODE,     Entry.bool(LsKey.ACTIVATE_SNYK_CODE, Preferences.ACTIVATE_SNYK_CODE_SECURITY, false));
        entries.put(LsKey.ACTIVATE_SNYK_OPEN_SOURCE, Entry.bool(LsKey.ACTIVATE_SNYK_OPEN_SOURCE, Preferences.ACTIVATE_SNYK_OPEN_SOURCE, true));
        entries.put(LsKey.ACTIVATE_SNYK_IAC,      Entry.bool(LsKey.ACTIVATE_SNYK_IAC, Preferences.ACTIVATE_SNYK_IAC, true));
        entries.put(LsKey.ACTIVATE_SNYK_SECRETS,  Entry.bool(LsKey.ACTIVATE_SNYK_SECRETS, Preferences.ACTIVATE_SNYK_SECRETS, false));
        entries.put(LsKey.SCANNING_MODE, new Entry(LsKey.SCANNING_MODE, Preferences.SCANNING_MODE_AUTOMATIC, "true",
                v -> Boolean.parseBoolean(v),
                value -> String.valueOf(Boolean.TRUE.equals(value)),
                false, false, new String[0],
                // The dialog's scanning-mode <select> is a data-bool control: it serializes as a
                // JSON boolean (true=auto, false=manual), not the legacy "auto"/"manual" string.
                // Accept the boolean; fall back to the "auto" string for older form payloads.
                node -> String.valueOf(node.isBoolean() ? node.booleanValue() : "auto".equals(node.asText())),
                false));
        entries.put(LsKey.ENABLE_DELTA_FINDINGS,  Entry.bool(LsKey.ENABLE_DELTA_FINDINGS, Preferences.ENABLE_DELTA, false));
        entries.put(LsKey.SEND_ERROR_REPORTS,     Entry.simple(LsKey.SEND_ERROR_REPORTS, Preferences.SEND_ERROR_REPORTS, ""));
        entries.put(LsKey.MANAGE_BINARIES_AUTOMATICALLY, Entry.boolFallback(LsKey.MANAGE_BINARIES_AUTOMATICALLY, Preferences.MANAGE_BINARIES_AUTOMATICALLY, true));
        entries.put(LsKey.CLI_PATH,               Entry.fallback(LsKey.CLI_PATH, Preferences.CLI_PATH, ""));
        entries.put(LsKey.USER_SETTINGS_PATH,     Entry.simple(LsKey.USER_SETTINGS_PATH, Preferences.PATH_KEY, ""));
        entries.put(LsKey.CLI_BASE_DOWNLOAD_URL,  Entry.fallback(LsKey.CLI_BASE_DOWNLOAD_URL, Preferences.CLI_BASE_URL, Preferences.DEFAULT_CLI_BASE_URL));
        entries.put(LsKey.INSECURE,               Entry.boolFallback(LsKey.INSECURE, Preferences.INSECURE_KEY, false));
        entries.put(LsKey.ADDITIONAL_PARAMS,      Entry.simple(LsKey.ADDITIONAL_PARAMS, Preferences.ADDITIONAL_PARAMETERS, ""));
        entries.put(LsKey.ADDITIONAL_ENV,         Entry.simple(LsKey.ADDITIONAL_ENV, Preferences.ADDITIONAL_ENVIRONMENT, ""));
        entries.put(LsKey.ENABLE_TRUSTED_FOLDERS_FEATURE, Entry.fixed(LsKey.ENABLE_TRUSTED_FOLDERS_FEATURE, Boolean.TRUE.toString()));
        entries.put(LsKey.ISSUE_VIEW_OPEN_ISSUES, Entry.bool(LsKey.ISSUE_VIEW_OPEN_ISSUES, Preferences.FILTER_IGNORES_SHOW_OPEN_ISSUES, true));
        entries.put(LsKey.ISSUE_VIEW_IGNORED_ISSUES, Entry.bool(LsKey.ISSUE_VIEW_IGNORED_ISSUES, Preferences.FILTER_IGNORES_SHOW_IGNORED_ISSUES, false));
        // Severity filters share a changed flag: if any one changed, all 4 are sent changed=true.
        entries.put(LsKey.SEVERITY_FILTER_CRITICAL, new Entry(LsKey.SEVERITY_FILTER_CRITICAL, Preferences.FILTER_SHOW_CRITICAL, TRUE,
                v -> Boolean.parseBoolean(v), LsSettingsRegistry::defaultToString, false, false,
                new String[]{Preferences.FILTER_SHOW_HIGH, Preferences.FILTER_SHOW_MEDIUM, Preferences.FILTER_SHOW_LOW},
                null, false));
        entries.put(LsKey.SEVERITY_FILTER_HIGH, new Entry(LsKey.SEVERITY_FILTER_HIGH, Preferences.FILTER_SHOW_HIGH, TRUE,
                v -> Boolean.parseBoolean(v), LsSettingsRegistry::defaultToString, false, false,
                new String[]{Preferences.FILTER_SHOW_CRITICAL, Preferences.FILTER_SHOW_MEDIUM, Preferences.FILTER_SHOW_LOW},
                null, false));
        entries.put(LsKey.SEVERITY_FILTER_MEDIUM, new Entry(LsKey.SEVERITY_FILTER_MEDIUM, Preferences.FILTER_SHOW_MEDIUM, TRUE,
                v -> Boolean.parseBoolean(v), LsSettingsRegistry::defaultToString, false, false,
                new String[]{Preferences.FILTER_SHOW_CRITICAL, Preferences.FILTER_SHOW_HIGH, Preferences.FILTER_SHOW_LOW},
                null, false));
        entries.put(LsKey.SEVERITY_FILTER_LOW, new Entry(LsKey.SEVERITY_FILTER_LOW, Preferences.FILTER_SHOW_LOW, TRUE,
                v -> Boolean.parseBoolean(v), LsSettingsRegistry::defaultToString, false, false,
                new String[]{Preferences.FILTER_SHOW_CRITICAL, Preferences.FILTER_SHOW_HIGH, Preferences.FILTER_SHOW_MEDIUM},
                null, false));
        // risk_score_threshold: int or null (null = no threshold); "0" = no threshold in pref.
        entries.put(LsKey.RISK_SCORE_THRESHOLD, new Entry(LsKey.RISK_SCORE_THRESHOLD, Preferences.RISK_SCORE_THRESHOLD, "0",
                v -> {
                    try {
                        int i = Integer.parseInt(v);
                        return i > 0 ? Integer.valueOf(i) : null;
                    } catch (NumberFormatException e) { return null; }
                },
                value -> {
                    if (value == null) return "0";
                    if (value instanceof Number) {
                        long l = ((Number) value).longValue();
                        return l > 0 ? String.valueOf(l) : "0";
                    }
                    return String.valueOf(value);
                },
                false, false, new String[0],
                node -> node.isNull() ? "0" : String.valueOf(node.asInt()), false));
        // trusted_folders: always-changed array, also set top-level for InitializationOptions.
        entries.put(LsKey.TRUSTED_FOLDERS, new Entry(LsKey.TRUSTED_FOLDERS, Preferences.TRUSTED_FOLDERS, "",
                v -> {
                    if (v == null || v.isBlank()) return new String[0];
                    return v.split(java.io.File.pathSeparator);
                },
                value -> {
                    if (value == null) return "";
                    if (value instanceof java.util.List) {
                        return ((java.util.List<?>) value).stream()
                                .map(String::valueOf)
                                .filter(s -> !s.isBlank())
                                .collect(java.util.stream.Collectors.joining(java.io.File.pathSeparator));
                    }
                    return String.valueOf(value);
                },
                true, false, new String[0],
                node -> {
                    if (node.isNull()) return "";
                    StringBuilder sb = new StringBuilder();
                    for (com.fasterxml.jackson.databind.JsonNode el : node) {
                        if (sb.length() > 0) sb.append(java.io.File.pathSeparator);
                        sb.append(el.asText());
                    }
                    return sb.toString();
                }, false));
        entries.put(LsKey.CLI_RELEASE_CHANNEL,    Entry.fallback(LsKey.CLI_RELEASE_CHANNEL, Preferences.RELEASE_CHANNEL, Preferences.RELEASE_CHANNEL_STABLE));

        ENTRIES = Collections.unmodifiableMap(entries);

        // Build BY_LS_KEY from ENTRIES (all entries, including inbound deserialization).
        Map<String, Entry> map = new LinkedHashMap<>();
        for (Entry e : ENTRIES.values()) {
            map.put(e.lsKey.key, e);
        }
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
