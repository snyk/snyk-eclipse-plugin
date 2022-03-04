package io.snyk.languageserver.download;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.reset;
import static org.mockito.Mockito.when;

import org.apache.commons.lang3.SystemUtils;

class LsUtilsTest extends LsBaseTest {

	@Override
	@BeforeEach
	void setUp() {
		utils = new LsUtils();
	}

	@Test
	void testDownloadBinaryNameConstructions() {
		var actual = utils.getDownloadBinaryName("testVersion");
		String expected = "snyk-ls_testVersion_" + utils.getOs() + "_" + utils.getArch();
		if (expected.contains("windows"))
			expected += ".exe";
		assertEquals(expected, actual);
	}
}