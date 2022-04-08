package io.snyk.languageserver;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;

import java.io.File;
import java.io.IOException;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@SuppressWarnings("ResultOfMethodCallIgnored")
public class LsBaseTest {
    protected LsRuntimeEnvironment environment = null;

    private File lsFile = getTempFile();

    @BeforeEach
    protected void setUp() {
        if (lsFile.exists()) lsFile.delete();
        lsFile = getTempFile();
        environment = mock(LsRuntimeEnvironment.class);
        when(environment.getLSFile()).thenReturn(lsFile);
        when(environment.getArch()).thenReturn("amd64");
        when(environment.getOs()).thenReturn("linux");
        when(environment.getDownloadBinaryName(any())).thenReturn("snyk-ls_testVersion_linux_amd64");
    }

    @AfterEach
    void tearDown() {
        lsFile.delete();
    }

    protected File getTempFile() {
        try {
            File tempFile = File.createTempFile("ls-test", "tmp");
            tempFile.deleteOnExit();
            return tempFile;
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}
