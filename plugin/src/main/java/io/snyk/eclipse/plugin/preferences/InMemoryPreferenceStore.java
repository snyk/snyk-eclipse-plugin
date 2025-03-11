package io.snyk.eclipse.plugin.preferences;

import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang3.NotImplementedException;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.IPreferenceNodeVisitor;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.osgi.service.prefs.BackingStoreException;
import org.osgi.service.prefs.Preferences;

public class InMemoryPreferenceStore implements IPreferenceStore, IEclipsePreferences {

	private final Map<String, String> store = new HashMap<>();

	public InMemoryPreferenceStore() {
	}

	@Override
	public boolean getBoolean(String key, boolean defaultValue) {
		if (store.containsKey(key)) {
			return Boolean.parseBoolean(store.get(key));
		}
		return defaultValue;
	}

	@Override
	public void put(String key, String value) {
		store.put(key, value);
	}

	public String getString(String key, String defaultValue) {
		if (store.containsKey(key)) {
			return store.get(key);
		}
		return defaultValue;
	}

	public IPreferenceStore getStore() {
		throw new NotImplementedException();
	}

	@Override
	public void addPropertyChangeListener(IPropertyChangeListener listener) {
		// TODO Auto-generated method stub

	}

	@Override
	public boolean contains(String name) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public void firePropertyChangeEvent(String name, Object oldValue, Object newValue) {
		// TODO Auto-generated method stub

	}

	@Override
	public boolean getBoolean(String name) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean getDefaultBoolean(String name) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public double getDefaultDouble(String name) {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public float getDefaultFloat(String name) {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public int getDefaultInt(String name) {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public long getDefaultLong(String name) {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public String getDefaultString(String name) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public double getDouble(String name) {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public float getFloat(String name) {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public int getInt(String name) {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public long getLong(String name) {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public String getString(String name) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean isDefault(String name) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean needsSaving() {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public void putValue(String name, String value) {
		// TODO Auto-generated method stub

	}

	@Override
	public void removePropertyChangeListener(IPropertyChangeListener listener) {
		// TODO Auto-generated method stub

	}

	@Override
	public void setDefault(String name, double value) {
		// TODO Auto-generated method stub

	}

	@Override
	public void setDefault(String name, float value) {
		// TODO Auto-generated method stub

	}

	@Override
	public void setDefault(String name, int value) {
		// TODO Auto-generated method stub

	}

	@Override
	public void setDefault(String name, long value) {
		// TODO Auto-generated method stub

	}

	@Override
	public void setDefault(String name, String defaultObject) {
		// TODO Auto-generated method stub

	}

	@Override
	public void setDefault(String name, boolean value) {
		// TODO Auto-generated method stub

	}

	@Override
	public void setToDefault(String name) {
		// TODO Auto-generated method stub

	}

	@Override
	public void setValue(String name, double value) {
		// TODO Auto-generated method stub

	}

	@Override
	public void setValue(String name, float value) {
		// TODO Auto-generated method stub

	}

	@Override
	public void setValue(String name, int value) {
		// TODO Auto-generated method stub

	}

	@Override
	public void setValue(String name, long value) {
		// TODO Auto-generated method stub

	}

	@Override
	public void setValue(String name, String value) {
		// TODO Auto-generated method stub

	}

	@Override
	public void setValue(String name, boolean value) {
		// TODO Auto-generated method stub

	}

	@Override
	public String get(String key, String def) {
		if (this.store.containsKey(key)) {
			return store.get(key);
		} else
			return def;
	}

	@Override
	public void remove(String key) {
		// TODO Auto-generated method stub

	}

	@Override
	public void clear() throws BackingStoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public void putInt(String key, int value) {
		// TODO Auto-generated method stub

	}

	@Override
	public int getInt(String key, int def) {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public void putLong(String key, long value) {
		// TODO Auto-generated method stub

	}

	@Override
	public long getLong(String key, long def) {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public void putBoolean(String key, boolean value) {
		// TODO Auto-generated method stub

	}

	@Override
	public void putFloat(String key, float value) {
		// TODO Auto-generated method stub

	}

	@Override
	public float getFloat(String key, float def) {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public void putDouble(String key, double value) {
		// TODO Auto-generated method stub

	}

	@Override
	public double getDouble(String key, double def) {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public void putByteArray(String key, byte[] value) {
		// TODO Auto-generated method stub

	}

	@Override
	public byte[] getByteArray(String key, byte[] def) {
		// TODO Auto-generated method stub
		return new byte[0];
	}

	@Override
	public String[] keys() throws BackingStoreException {
		// TODO Auto-generated method stub
		return new String[0];
	}

	@Override
	public String[] childrenNames() throws BackingStoreException {
		// TODO Auto-generated method stub
		return new String[0];
	}

	@Override
	public Preferences parent() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean nodeExists(String pathName) throws BackingStoreException {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public String name() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String absolutePath() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void flush() throws BackingStoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public void sync() throws BackingStoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public void addNodeChangeListener(INodeChangeListener listener) {
		// TODO Auto-generated method stub

	}

	@Override
	public void removeNodeChangeListener(INodeChangeListener listener) {
		// TODO Auto-generated method stub

	}

	@Override
	public void addPreferenceChangeListener(IPreferenceChangeListener listener) {
		// TODO Auto-generated method stub

	}

	@Override
	public void removePreferenceChangeListener(IPreferenceChangeListener listener) {
		// TODO Auto-generated method stub

	}

	@Override
	public void removeNode() throws BackingStoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public Preferences node(String path) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void accept(IPreferenceNodeVisitor visitor) throws BackingStoreException {
		// TODO Auto-generated method stub

	}
}
