package io.snyk.eclipse.plugin.properties;

import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.osgi.service.prefs.BackingStoreException;

import io.snyk.eclipse.plugin.preferences.PreferenceStore;
import io.snyk.eclipse.plugin.utils.SnykLogger;

public class PreferencesToPreferenceStoreWrapper implements IPreferenceStore, PreferenceStore {

	private final IEclipsePreferences preferences;
	private final Set<IPropertyChangeListener> listeners = new HashSet<>();
	private Lock listenerLock = new ReentrantLock();

	public PreferencesToPreferenceStoreWrapper(IEclipsePreferences preferences) {
		this.preferences = preferences;
	}

	@Override
	public void addPropertyChangeListener(IPropertyChangeListener listener) {
		try {
			listenerLock.lock();
			this.listeners.add(listener);
		} finally {
			listenerLock.unlock();
		}
	}

	@Override
	public boolean contains(String name) {
		return !preferences.get(name, "").isBlank();
	}

	@Override
	public void firePropertyChangeEvent(String name, Object oldValue, Object newValue) {
		var event = new PropertyChangeEvent(this, name, oldValue, newValue);
		try {
			listenerLock.lock();
			for (IPropertyChangeListener listener : listeners) {
				listener.propertyChange(event);
			}
		} finally {
			listenerLock.unlock();
		}
	}

	@Override
	public boolean getBoolean(String name) {
		return this.preferences.getBoolean(name, BOOLEAN_DEFAULT_DEFAULT);
	}

	@Override
	public boolean getDefaultBoolean(String name) {
		return false;
	}

	@Override
	public double getDefaultDouble(String name) {
		return 0;
	}

	@Override
	public float getDefaultFloat(String name) {
		return 0;
	}

	@Override
	public int getDefaultInt(String name) {
		return 0;
	}

	@Override
	public long getDefaultLong(String name) {
		return 0;
	}

	@Override
	public String getDefaultString(String name) {
		return "";
	}

	@Override
	public double getDouble(String name) {
		return preferences.getDouble(name, DOUBLE_DEFAULT_DEFAULT);
	}

	@Override
	public float getFloat(String name) {
		return preferences.getFloat(name, FLOAT_DEFAULT_DEFAULT);
	}

	@Override
	public int getInt(String name) {
		return preferences.getInt(name, INT_DEFAULT_DEFAULT);
	}

	@Override
	public long getLong(String name) {
		return preferences.getLong(name, LONG_DEFAULT_DEFAULT);
	}

	@Override
	public String getString(String name) {
		return preferences.get(name, "");
	}

	@Override
	public boolean isDefault(String name) {
		return false;
	}

	@Override
	public boolean needsSaving() {
		return true;
	}

	@Override
	public void putValue(String name, String value) {
		this.preferences.put(name, value);
		flush();
	}

	private void flush() {
		try {
			this.preferences.flush();
		} catch (BackingStoreException e) {
			SnykLogger.logError(e);
		}
	}

	@Override
	public void removePropertyChangeListener(IPropertyChangeListener listener) {
		try {
			listenerLock.lock();
			listeners.remove(listener);
		} finally {
			listenerLock.unlock();
		}
	}

	@Override
	public void setDefault(String name, double value) {
	}

	@Override
	public void setDefault(String name, float value) {
	}

	@Override
	public void setDefault(String name, int value) {
	}

	@Override
	public void setDefault(String name, long value) {
	}

	@Override
	public void setDefault(String name, String defaultObject) {
	}

	@Override
	public void setDefault(String name, boolean value) {
	}

	@Override
	public void setToDefault(String name) {
		preferences.put(name, STRING_DEFAULT_DEFAULT);
	}

	@Override
	public void setValue(String name, double value) {
		preferences.putDouble(name, value);
		flush();
	}

	@Override
	public void setValue(String name, float value) {
		preferences.putFloat(name, value);
		flush();
	}

	@Override
	public void setValue(String name, int value) {
		preferences.putInt(name, value);
		flush();
	}

	@Override
	public void setValue(String name, long value) {
		preferences.putLong(name, value);
		flush();
	}

	@Override
	public void setValue(String name, String value) {
		preferences.put(name, value);
		flush();
	}

	@Override
	public void setValue(String name, boolean value) {
		preferences.putBoolean(name, value);
		flush();
	}

	@Override
	public boolean getBoolean(String key, boolean defaultValue) {
		return preferences.getBoolean(key, defaultValue);
	}

	@Override
	public void put(String key, String value) {
		preferences.put(key, value);
		flush();
	}

	@Override
	public String getString(String key, String defaultValue) {
		return preferences.get(key, defaultValue);
	}

	@Override
	public IPreferenceStore getStore() {
		return this;
	}
}
