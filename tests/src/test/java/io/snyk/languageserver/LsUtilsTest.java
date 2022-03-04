package io.snyk.languageserver;

import io.snyk.eclipse.plugin.properties.Preferences;
import io.snyk.eclipse.plugin.utils.FileSystemUtil;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.File;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class LsUtilsTest extends LsBaseTest {
    private Preferences preferenceMock = null;

    @Override
    @BeforeEach
    protected void setUp() {
        preferenceMock = mock(Preferences.class);
        utils = new LsUtils(preferenceMock);
    }

    @Test
    void testDownloadBinaryNameConstructions() {
        var actual = utils.getDownloadBinaryName("testVersion");
        String expected = "snyk-ls_testVersion_" + utils.getOs() + "_" + utils.getArch();
        if (expected.contains("windows"))
            expected += ".exe";
        assertEquals(expected, actual);
    }

    @Test
    void testGetLsFileShouldReturnPreferenceIfSet() {
        String expectedLsPath = "testPath";
        when(preferenceMock.getLsBinary()).thenReturn(expectedLsPath);

        File file = utils.getLSFile();

        assertEquals(expectedLsPath, file.getPath());
    }

    @Test
    void testGetLsFileShouldReturnDefaultIfPreferenceNotSet() {
        when(preferenceMock.getLsBinary()).thenReturn(null);

        File file = utils.getLSFile();

        assertEquals(new File(FileSystemUtil.getCliDirectory(), utils.getBinaryName()), file);
    }
}