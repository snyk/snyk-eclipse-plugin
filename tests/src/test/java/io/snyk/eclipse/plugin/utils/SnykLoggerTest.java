package io.snyk.eclipse.plugin.utils;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.times;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.ui.statushandlers.StatusManager;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

class SnykLoggerTest {

    @Test
    void logAndShowWarning_usesSeverityWarning() {
        try (var mockedStatusManager = mockStatic(StatusManager.class)) {
            var managerMock = org.mockito.Mockito.mock(StatusManager.class);
            mockedStatusManager.when(StatusManager::getManager).thenReturn(managerMock);

            SnykLogger.logAndShowWarning("CLI download failed: connection timed out. Will try to start with existing binary if available.");

            var captor = ArgumentCaptor.forClass(IStatus.class);
            org.mockito.Mockito.verify(managerMock, times(1))
                    .handle(captor.capture(), eq(StatusManager.LOG | StatusManager.SHOW));

            IStatus captured = captor.getValue();
            assertEquals(IStatus.WARNING, captured.getSeverity(),
                    "logAndShowWarning must produce WARNING severity so the user notification is not styled as INFO");
            assertEquals("CLI download failed: connection timed out. Will try to start with existing binary if available.",
                    captured.getMessage());
        }
    }
}
