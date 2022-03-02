package io.snyk.languageserver.download;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;

import java.io.File;
import java.io.IOException;
import java.time.Instant;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.reset;
import static org.mockito.Mockito.when;

@SuppressWarnings("ResultOfMethodCallIgnored")
public class LsBaseTest {    
    protected LsUtils utils = null;

    private File lsFile = getTempFile();

    @BeforeEach
    void setUp() {    	
        if (lsFile.exists()) lsFile.delete();
        lsFile = getTempFile();
        utils = mock(LsUtils.class);
        when(utils.getLSFile()).thenReturn(lsFile);
        when(utils.getArch()).thenReturn("amd64");
        when(utils.getOs()).thenReturn("linux");
    }

    @AfterEach
    void tearDown() {
        lsFile.delete();
    }

    protected File getTempFile() {
        try {
            File tempFile = File.createTempFile("ls-test","tmp");
            tempFile.deleteOnExit();
            return tempFile;
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}
