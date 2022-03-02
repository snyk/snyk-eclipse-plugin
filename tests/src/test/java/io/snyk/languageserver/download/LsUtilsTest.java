package io.snyk.languageserver.download;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class LsUtilsTest extends LsBaseTest {

    @Override
    @BeforeEach
    void setUp() {
        utils = new LsUtils();
    }

    @Test
    void shouldBeCaseInsensitiveRegardingArchAndAddDotExeForWindows() {
        String osArch = "aMd64";
        String osName = "WiNdOWS";

        assertEquals("snyk-lsp.windows.amd64.exe", utils.getBinaryName(osArch, osName));
    }
}