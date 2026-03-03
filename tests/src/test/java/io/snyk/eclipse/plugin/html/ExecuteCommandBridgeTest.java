package io.snyk.eclipse.plugin.html;

import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

class ExecuteCommandBridgeTest {

  @Test
  void buildClientScript_containsIdeExecuteCommandDefinition() {
    String script = ExecuteCommandBridge.buildClientScript();
    assertTrue(script.contains("window.__ideExecuteCommand__"),
        "Should define window.__ideExecuteCommand__");
  }

  @Test
  void buildClientScript_containsIdeCallbacksInitialization() {
    String script = ExecuteCommandBridge.buildClientScript();
    assertTrue(script.contains("window.__ideCallbacks__"),
        "Should initialize window.__ideCallbacks__");
  }

  @Test
  void buildClientScript_callsBridgeFunction() {
    String script = ExecuteCommandBridge.buildClientScript();
    assertTrue(script.contains("__ideExecuteCommandBridge__"),
        "Should call __ideExecuteCommandBridge__ native function");
  }

  @Test
  void buildClientScript_handlesCallbackRegistration() {
    String script = ExecuteCommandBridge.buildClientScript();
    assertTrue(script.contains("callback"), "Should register callback");
    assertTrue(script.contains("callbackId"), "Should use callbackId");
  }

  @Test
  void injectScript_doesNotThrowWhenBrowserIsNull() {
    // should not throw
    ExecuteCommandBridge.injectScript(null);
  }
}
