package io.snyk.eclipse.plugin.properties;

import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.equinox.security.storage.ISecurePreferences;
import org.eclipse.equinox.security.storage.StorageException;
import org.eclipse.ui.preferences.ScopedPreferenceStore;

public class SnykSecurePreferenceStore extends ScopedPreferenceStore {
    private SnykSecurePreferenceStore instance;
    private final ISecurePreferences node;

    public SnykSecurePreferenceStore(ISecurePreferences node, String qualifier) {
        super(InstanceScope.INSTANCE, qualifier);
        this.node = node;
    }

    @Override
    public double getDouble(String name) {
        try {
            return node.getDouble(name, DOUBLE_DEFAULT_DEFAULT);
        } catch (StorageException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public float getFloat(String name) {
        try {
            return node.getFloat(name, FLOAT_DEFAULT_DEFAULT);
        } catch (StorageException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public int getInt(String name) {
        try {
            return node.getInt(name, INT_DEFAULT_DEFAULT);
        } catch (StorageException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public long getLong(String name) {
        try {
            return node.getLong(name, LONG_DEFAULT_DEFAULT);
        } catch (StorageException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public String getString(String name) {
        try {
            return node.get(name, STRING_DEFAULT_DEFAULT);
        } catch (StorageException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void putValue(String name, String value) {
        try {
            node.put(name, value, true);
        } catch (StorageException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void setValue(String name, double value) {
        try {
            node.putDouble(name, value, true);
        } catch (StorageException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void setValue(String name, float value) {
        try {
            node.putFloat(name, value, true);
        } catch (StorageException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void setValue(String name, int value) {
        try {
            node.putInt(name, value, true);
        } catch (StorageException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void setValue(String name, long value) {
        try {
            node.putLong(name, value, true);
        } catch (StorageException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void setValue(String name, String value) {
        try {
            node.put(name, value, true);
        } catch (StorageException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void setValue(String name, boolean value) {
        try {
            node.putBoolean(name, value, true);
        } catch (StorageException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public boolean contains(String name) {
        return node.nodeExists(name);
    }

    @Override
    public boolean getBoolean(String name) {
        try {
            return node.getBoolean(name, BOOLEAN_DEFAULT_DEFAULT);
        } catch (StorageException e) {
            throw new RuntimeException(e);
        }
    }
}
