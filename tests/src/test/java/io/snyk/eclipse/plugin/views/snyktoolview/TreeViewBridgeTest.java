package io.snyk.eclipse.plugin.views.snyktoolview;

import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import io.snyk.eclipse.plugin.html.ExecuteCommandBridge;
import io.snyk.languageserver.LsBaseTest;

class TreeViewBridgeTest extends LsBaseTest {

	// T-I-002: bridge client script defines window.__ideExecuteCommand__ used by the
	// tree HTML to call snyk.navigateToRange([path, range], null)
	@Test
	void clientScript_definesIdeExecuteCommand() {
		String script = ExecuteCommandBridge.buildClientScript();
		assertTrue(script.contains("window.__ideExecuteCommand__"),
				"Client script must define window.__ideExecuteCommand__");
	}

	// T-I-002: bridge client script delegates to __ideExecuteCommandBridge__ native function
	@Test
	void clientScript_delegatesToNativeBridgeFunction() {
		String script = ExecuteCommandBridge.buildClientScript();
		assertTrue(script.contains("__ideExecuteCommandBridge__"),
				"Client script must call __ideExecuteCommandBridge__");
	}

	// T-I-003: client script passes args via JSON.stringify so batch args like
	// [[id1,true],[id2,false]] for snyk.setNodeExpanded are correctly serialised
	@Test
	void clientScript_serialisesArgsViaJsonStringify() {
		String script = ExecuteCommandBridge.buildClientScript();
		assertTrue(script.contains("JSON.stringify"),
				"Client script must use JSON.stringify to serialise args including batch payloads");
	}

	// T-I-003: client script registers callbacks so results from snyk.setNodeExpanded
	// can be returned to the JS caller
	@Test
	void clientScript_registersCallbacksForReturnValues() {
		String script = ExecuteCommandBridge.buildClientScript();
		assertTrue(script.contains("window.__ideCallbacks__"),
				"Client script must register callbacks for command results");
	}

	// Verify the JS wrapper includes the callbackId forwarding
	@Test
	void clientScript_forwardsCallbackIdToBridge() {
		String script = ExecuteCommandBridge.buildClientScript();
		assertTrue(script.contains("callbackId"),
				"Client script must forward callbackId to the bridge function");
	}
}
