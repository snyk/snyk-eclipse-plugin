package io.snyk.eclipse.plugin.utils;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

import io.snyk.eclipse.plugin.runner.ProcessResult;

public class MockHandler {
	
	//set to fake mock the CLI calls
	public static final boolean MOCK = false;
	
	public static ProcessResult getMockContent(String result) {
		return new ProcessResult(result, null);
	}
	
	public static ProcessResult getMockScanResult() {
		return getMockContent(MOCK_SCAN_RESULT);
	}
	
	private static String getResource() {
		try {
			return new String(Files.readAllBytes(Paths.get("some_output.json")));
		} catch (IOException e) {
			e.printStackTrace();
			return "error";
		}
		 
	}
		
	private static final String MOCK_SCAN_RESULT = "{\n" + 
			"  \"ok\": false,\n" + 
			"  \"vulnerabilities\": [\n" + 
			"    {\n" + 
			"      \"CVSSv3\": \"CVSS:3.0/AV:N/AC:L/PR:N/UI:N/S:U/C:L/I:L/A:L\",\n" + 
			"      \"alternativeIds\": [],\n" + 
			"      \"creationTime\": \"2019-02-14T16:46:18.024227Z\",\n" + 
			"      \"credit\": [\n" + 
			"        \"Mahmoud Gamal\",\n" + 
			"        \"Matias Lang\"\n" + 
			"      ],\n" + 
			"      \"cvssScore\": 7.3,\n" + 
			"      \"description\": \"## Overview\\r\\n\\r\\n[handlebars](https://www.npmjs.com/package/handlebars) is a extension to the Mustache templating language.\\r\\n\\r\\n\\r\\nAffected versions of this package are vulnerable to Prototype Pollution.\\r\\nTemplates may alter an Objects' prototype, thus allowing an attacker to execute arbitrary code on the server.\\r\\n\\r\\n## Details\\r\\nPrototype Pollution is a vulnerability affecting JavaScript. Prototype Pollution refers to the ability to inject properties into existing JavaScript language construct prototypes, such as objects. JavaScript allows all Object attributes to be altered, including their magical attributes such as `_proto_`, `constructor` and `prototype`. An attacker manipulates these attributes to overwrite, or pollute, a JavaScript application object prototype of the base object by injecting other values.  Properties on the `Object.prototype` are then inherited by all the JavaScript objects through the prototype chain. When that happens, this leads to either denial of service by triggering JavaScript exceptions, or it tampers with the application source code to force the code path that the attacker injects, thereby leading to remote code execution.\\r\\n\\r\\nThere are two main ways in which the pollution of prototypes occurs:\\r\\n\\r\\n-   Unsafe `Object` recursive merge\\r\\n    \\r\\n-   Property definition by path\\r\\n    \\r\\n\\r\\n### Unsafe Object recursive merge\\r\\n\\r\\nThe logic of a vulnerable recursive merge function follows the following high-level model:\\r\\n```\\r\\nmerge (target, source)\\r\\n\\r\\nforeach property of source\\r\\n\\r\\nif property exists and is an object on both the target and the source\\r\\n\\r\\nmerge(target[property], source[property])\\r\\n\\r\\nelse\\r\\n\\r\\ntarget[property] = source[property]\\r\\n```\\r\\n<br>  \\r\\n\\r\\nWhen the source object contains a property named `_proto_` defined with `Object.defineProperty()` , the condition that checks if the property exists and is an object on both the target and the source passes and the merge recurses with the target, being the prototype of `Object` and the source of `Object` as defined by the attacker. Properties are then copied on the `Object` prototype.\\r\\n\\r\\nClone operations are a special sub-class of unsafe recursive merges, which occur when a recursive merge is conducted on an empty object: `merge({},source)`.\\r\\n\\r\\n`lodash` and `Hoek` are examples of libraries susceptible to recursive merge attacks.\\r\\n\\r\\n### Property definition by path\\r\\n\\r\\nThere are a few JavaScript libraries that use an API to define property values on an object based on a given path. The function that is generally affected contains this signature: `theFunction(object, path, value)`\\r\\n\\r\\nIf the attacker can control the value of “path”, they can set this value to `_proto_.myValue`. `myValue` is then assigned to the prototype of the class of the object.\\r\\n\\r\\n## Types of attacks\\r\\n\\r\\nThere are a few methods by which Prototype Pollution can be manipulated:\\r\\n\\r\\n| Type |Origin  |Short description |\\r\\n|--|--|--|\\r\\n| **Denial of service (DoS)**|Client  |This is the most likely attack. <br>DoS occurs when `Object` holds generic functions that are implicitly called for various operations (for example, `toString` and `valueOf`). <br> The attacker pollutes `Object.prototype.someattr` and alters its state to an unexpected value such as `Int` or `Object`. In this case, the code fails and is likely to cause a denial of service.  <br>**For example:** if an attacker pollutes `Object.prototype.toString` by defining it as an integer, if the codebase at any point was reliant on `someobject.toString()` it would fail. |\\r\\n |**Remote Code Execution**|Client|Remote code execution is generally only possible in cases where the codebase evaluates a specific attribute of an object, and then executes that evaluation.<br>**For example:** `eval(someobject.someattr)`. In this case, if the attacker pollutes `Object.prototype.someattr` they are likely to be able to leverage this in order to execute code.|\\r\\n|**Property Injection**|Client|The attacker pollutes properties that the codebase relies on for their informative value, including security properties such as cookies or tokens.<br>  **For example:** if a codebase checks privileges for `someuser.isAdmin`, then when the attacker pollutes `Object.prototype.isAdmin` and sets it to equal `true`, they can then achieve admin privileges.|\\r\\n\\r\\n## Affected environments\\r\\n\\r\\nThe following environments are susceptible to a Prototype Pollution attack:\\r\\n\\r\\n-   Application server\\r\\n    \\r\\n-   Web server\\r\\n    \\r\\n\\r\\n## How to prevent\\r\\n\\r\\n1.  Freeze the prototype— use `Object.freeze (Object.prototype)`.\\r\\n    \\r\\n2.  Require schema validation of JSON input.\\r\\n    \\r\\n3.  Avoid using unsafe recursive merge functions.\\r\\n    \\r\\n4.  Consider using objects without prototypes (for example, `Object.create(null)`), breaking the prototype chain and preventing pollution.\\r\\n    \\r\\n5.  As a best practice use `Map` instead of `Object`.\\r\\n\\r\\n### For more information on this vulnerability type:\\r\\n\\r\\n[Arteau, Oliver. “JavaScript prototype pollution attack in NodeJS application.” GitHub, 26 May 2018](https://github.com/HoLyVieR/prototype-pollution-nsec18/blob/master/paper/JavaScript_prototype_pollution_attack_in_NodeJS.pdf)\\r\\n\\r\\n## Remediation\\r\\n\\r\\nUpgrade `handlebars` to version 4.0.13 or higher.\\r\\n\\r\\n\\r\\n## References\\r\\n\\r\\n- [GitHub Commit](https://github.com/wycats/handlebars.js/commit/7372d4e9dffc9d70c09671aa28b9392a1577fd86)\\r\\n\",\n" + 
			"      \"disclosureTime\": \"2018-12-28T20:34:57Z\",\n" + 
			"      \"fixedIn\": [\n" + 
			"        \"4.0.13\"\n" + 
			"      ],\n" + 
			"      \"functions\": [\n" + 
			"        {\n" + 
			"          \"functionId\": {\n" + 
			"            \"className\": null,\n" + 
			"            \"filePath\": \"dist/amd/handlebars/compiler/javascript-compiler.js\",\n" + 
			"            \"functionName\": \"JavaScriptCompiler.prototype.nameLookup\"\n" + 
			"          },\n" + 
			"          \"version\": [\n" + 
			"            \">1.0.12 <4.0.13\"\n" + 
			"          ]\n" + 
			"        },\n" + 
			"        {\n" + 
			"          \"functionId\": {\n" + 
			"            \"className\": null,\n" + 
			"            \"filePath\": \"dist/handlebars.js\",\n" + 
			"            \"functionName\": \"JavaScriptCompiler.Handlebars.JavaScriptCompiler\"\n" + 
			"          },\n" + 
			"          \"version\": [\n" + 
			"            \">=1.0.6 <=1.0.12\"\n" + 
			"          ]\n" + 
			"        }\n" + 
			"      ],\n" + 
			"      \"id\": \"SNYK-JS-HANDLEBARS-173692\",\n" + 
			"      \"identifiers\": {\n" + 
			"        \"CVE\": [],\n" + 
			"        \"CWE\": [\n" + 
			"          \"CWE-471\"\n" + 
			"        ],\n" + 
			"        \"NSP\": [\n" + 
			"          755\n" + 
			"        ]\n" + 
			"      },\n" + 
			"      \"language\": \"js\",\n" + 
			"      \"modificationTime\": \"2019-04-14T11:09:52.197745Z\",\n" + 
			"      \"moduleName\": \"handlebars\",\n" + 
			"      \"packageManager\": \"npm\",\n" + 
			"      \"packageName\": \"handlebars\",\n" + 
			"      \"patches\": [],\n" + 
			"      \"publicationTime\": \"2019-02-14T17:52:50Z\",\n" + 
			"      \"references\": [\n" + 
			"        {\n" + 
			"          \"title\": \"GitHub Commit\",\n" + 
			"          \"url\": \"https://github.com/wycats/handlebars.js/commit/7372d4e9dffc9d70c09671aa28b9392a1577fd86\"\n" + 
			"        }\n" + 
			"      ],\n" + 
			"      \"semver\": {\n" + 
			"        \"vulnerable\": [\n" + 
			"          \"<4.0.13\"\n" + 
			"        ]\n" + 
			"      },\n" + 
			"      \"severity\": \"high\",\n" + 
			"      \"title\": \"Prototype Pollution\",\n" + 
			"      \"from\": [\n" + 
			"        \"goof@1.0.1\",\n" + 
			"        \"tap@5.8.0\",\n" + 
			"        \"nyc@6.6.1\",\n" + 
			"        \"istanbul@0.4.3\",\n" + 
			"        \"handlebars@4.0.5\"\n" + 
			"      ],\n" + 
			"      \"upgradePath\": [\n" + 
			"        false,\n" + 
			"        \"tap@5.8.0\",\n" + 
			"        \"nyc@6.6.1\",\n" + 
			"        \"istanbul@0.4.3\",\n" + 
			"        \"handlebars@4.0.13\"\n" + 
			"      ],\n" + 
			"      \"isUpgradable\": true,\n" + 
			"      \"isPatchable\": false,\n" + 
			"      \"name\": \"handlebars\",\n" + 
			"      \"version\": \"4.0.5\"\n" + 
			"    },{\n" + 
			"      \"CVSSv3\": \"CVSS:3.0/AV:N/AC:H/PR:N/UI:N/S:U/C:N/I:N/A:L\",\n" + 
			"      \"alternativeIds\": [\n" + 
			"        \"SNYK-JS-MS-10509\"\n" + 
			"      ],\n" + 
			"      \"creationTime\": \"2017-04-12T10:02:45.497000Z\",\n" + 
			"      \"credit\": [\n" + 
			"        \"Snyk Security Research Team\"\n" + 
			"      ],\n" + 
			"      \"cvssScore\": 3.7,\n" + 
			"      \"description\": \"## Overview\\r\\n[`ms`](https://www.npmjs.com/package/ms) is a tiny millisecond conversion utility.\\r\\n\\r\\nAffected versions of this package are vulnerable to Regular Expression Denial of Service (ReDoS) due to an incomplete fix for previously reported vulnerability [npm:ms:20151024](https://snyk.io/vuln/npm:ms:20151024). The fix limited the length of accepted input string to 10,000 characters, and turned to be insufficient making it possible to block the event loop for 0.3 seconds (on a typical laptop) with a specially crafted string passed to `ms()` function.\\r\\n\\r\\n*Proof of concept*\\r\\n```js\\r\\nms = require('ms');\\r\\nms('1'.repeat(9998) + 'Q') // Takes about ~0.3s\\r\\n```\\r\\n\\r\\n**Note:** Snyk's patch for this vulnerability limits input length to 100 characters. This new limit was deemed to be a breaking change by the author.\\r\\nBased on user feedback, we believe the risk of breakage is _very_ low, while the value to your security is much greater, and therefore opted to still capture this change in a patch for earlier versions as well.  Whenever patching security issues, we always suggest to run tests on your code to validate that nothing has been broken.\\r\\n\\r\\nFor more information on `Regular Expression Denial of Service (ReDoS)` attacks, go to our [blog](https://snyk.io/blog/redos-and-catastrophic-backtracking/).\\r\\n\\r\\n## Disclosure Timeline\\r\\n- Feb 9th, 2017 - Reported the issue to package owner.\\r\\n- Feb 11th, 2017 - Issue acknowledged by package owner.\\r\\n- April 12th, 2017 - Fix PR opened by Snyk Security Team.\\r\\n- May 15th, 2017 - Vulnerability published.\\r\\n- May 16th, 2017 - Issue fixed and version `2.0.0` released.\\r\\n- May 21th, 2017 - Patches released for versions `>=0.7.1, <=1.0.0`.\\r\\n\\r\\n## Details\\r\\nDenial of Service (DoS) describes a family of attacks, all aimed at making a system inaccessible to its original and legitimate users. There are many types of DoS attacks, ranging from trying to clog the network pipes to the system by generating a large volume of traffic from many machines (a Distributed Denial of Service - DDoS - attack) to sending crafted requests that cause a system to crash or take a disproportional amount of time to process.\\r\\n\\r\\nThe Regular expression Denial of Service (ReDoS) is a type of Denial of Service attack. Regular expressions are incredibly powerful, but they aren't very intuitive and can ultimately end up making it easy for attackers to take your site down.\\r\\n\\r\\nLet’s take the following regular expression as an example:\\r\\n```js\\r\\nregex = /A(B|C+)+D/\\r\\n```\\r\\n\\r\\nThis regular expression accomplishes the following:\\r\\n- `A` The string must start with the letter 'A'\\r\\n- `(B|C+)+` The string must then follow the letter A with either the letter 'B' or some number of occurrences of the letter 'C' (the `+` matches one or more times). The `+` at the end of this section states that we can look for one or more matches of this section.\\r\\n- `D` Finally, we ensure this section of the string ends with a 'D'\\r\\n\\r\\nThe expression would match inputs such as `ABBD`, `ABCCCCD`, `ABCBCCCD` and `ACCCCCD`\\r\\n\\r\\nIt most cases, it doesn't take very long for a regex engine to find a match:\\r\\n\\r\\n```bash\\r\\n$ time node -e '/A(B|C+)+D/.test(\\\"ACCCCCCCCCCCCCCCCCCCCCCCCCCCCD\\\")'\\r\\n0.04s user 0.01s system 95% cpu 0.052 total\\r\\n\\r\\n$ time node -e '/A(B|C+)+D/.test(\\\"ACCCCCCCCCCCCCCCCCCCCCCCCCCCCX\\\")'\\r\\n1.79s user 0.02s system 99% cpu 1.812 total\\r\\n```\\r\\n\\r\\nThe entire process of testing it against a 30 characters long string takes around ~52ms. But when given an invalid string, it takes nearly two seconds to complete the test, over ten times as long as it took to test a valid string. The dramatic difference is due to the way regular expressions get evaluated.\\r\\n\\r\\nMost Regex engines will work very similarly (with minor differences). The engine will match the first possible way to accept the current character and proceed to the next one. If it then fails to match the next one, it will backtrack and see if there was another way to digest the previous character. If it goes too far down the rabbit hole only to find out the string doesn’t match in the end, and if many characters have multiple valid regex paths, the number of backtracking steps can become very large, resulting in what is known as _catastrophic backtracking_.\\r\\n\\r\\nLet's look at how our expression runs into this problem, using a shorter string: \\\"ACCCX\\\". While it seems fairly straightforward, there are still four different ways that the engine could match those three C's:\\r\\n1. CCC\\r\\n2. CC+C\\r\\n3. C+CC\\r\\n4. C+C+C.\\r\\n\\r\\nThe engine has to try each of those combinations to see if any of them potentially match against the expression. When you combine that with the other steps the engine must take, we can use [RegEx 101 debugger](https://regex101.com/debugger) to see the engine has to take a total of 38 steps before it can determine the string doesn't match.\\r\\n\\r\\nFrom there, the number of steps the engine must use to validate a string just continues to grow.\\r\\n\\r\\n| String | Number of C's | Number of steps |\\r\\n| -------|-------------:| -----:|\\r\\n| ACCCX | 3 | 38\\r\\n| ACCCCX | 4 | 71\\r\\n| ACCCCCX | 5 | 136\\r\\n| ACCCCCCCCCCCCCCX | 14 | 65,553\\r\\n\\r\\n\\r\\nBy the time the string includes 14 C's, the engine has to take over 65,000 steps just to see if the string is valid. These extreme situations can cause them to work very slowly (exponentially related to input size, as shown above), allowing an attacker to exploit this and can cause the service to excessively consume CPU, resulting in a Denial of Service.\\r\\n\\r\\n\\r\\n## Remediation\\r\\nUpgrade `ms` to version 2.0.0 or higher.\\r\\n\\r\\n## References\\r\\n- [GitHub PR](https://github.com/zeit/ms/pull/89)\\r\\n- [GitHub Commit](https://github.com/zeit/ms/pull/89/commits/305f2ddcd4eff7cc7c518aca6bb2b2d2daad8fef)\",\n" + 
			"      \"disclosureTime\": \"2017-04-11T21:00:00Z\",\n" + 
			"      \"fixedIn\": [\n" + 
			"        \"2.0.0\"\n" + 
			"      ],\n" + 
			"      \"functions\": [\n" + 
			"        {\n" + 
			"          \"functionId\": {\n" + 
			"            \"className\": null,\n" + 
			"            \"filePath\": \"index.js\",\n" + 
			"            \"functionName\": \"parse\"\n" + 
			"          },\n" + 
			"          \"version\": [\n" + 
			"            \"<2.0.0\"\n" + 
			"          ]\n" + 
			"        },\n" + 
			"        {\n" + 
			"          \"functionId\": {\n" + 
			"            \"className\": null,\n" + 
			"            \"filePath\": \"ms.js\",\n" + 
			"            \"functionName\": \"parse\"\n" + 
			"          },\n" + 
			"          \"version\": [\n" + 
			"            \">0.1.0 <=0.3.0\"\n" + 
			"          ]\n" + 
			"        }\n" + 
			"      ],\n" + 
			"      \"id\": \"npm:ms:20170412\",\n" + 
			"      \"identifiers\": {\n" + 
			"        \"ALTERNATIVE\": [\n" + 
			"          \"SNYK-JS-MS-10509\"\n" + 
			"        ],\n" + 
			"        \"CVE\": [],\n" + 
			"        \"CWE\": [\n" + 
			"          \"CWE-400\"\n" + 
			"        ]\n" + 
			"      },\n" + 
			"      \"language\": \"js\",\n" + 
			"      \"modificationTime\": \"2019-03-05T10:48:11.179906Z\",\n" + 
			"      \"moduleName\": \"ms\",\n" + 
			"      \"packageManager\": \"npm\",\n" + 
			"      \"packageName\": \"ms\",\n" + 
			"      \"patches\": [\n" + 
			"        {\n" + 
			"          \"comments\": [],\n" + 
			"          \"id\": \"patch:npm:ms:20170412:0\",\n" + 
			"          \"modificationTime\": \"2018-09-04T11:57:08.694191Z\",\n" + 
			"          \"urls\": [\n" + 
			"            \"https://s3.amazonaws.com/snyk-rules-pre-repository/snapshots/master/patches/npm/ms/20170412/ms_100.patch\"\n" + 
			"          ],\n" + 
			"          \"version\": \"=1.0.0\"\n" + 
			"        },\n" + 
			"        {\n" + 
			"          \"comments\": [],\n" + 
			"          \"id\": \"patch:npm:ms:20170412:1\",\n" + 
			"          \"modificationTime\": \"2018-09-04T11:57:08.695549Z\",\n" + 
			"          \"urls\": [\n" + 
			"            \"https://s3.amazonaws.com/snyk-rules-pre-repository/snapshots/master/patches/npm/ms/20170412/ms_072-073.patch\"\n" + 
			"          ],\n" + 
			"          \"version\": \"=0.7.2 || =0.7.3\"\n" + 
			"        },\n" + 
			"        {\n" + 
			"          \"comments\": [],\n" + 
			"          \"id\": \"patch:npm:ms:20170412:2\",\n" + 
			"          \"modificationTime\": \"2018-09-04T11:57:08.696913Z\",\n" + 
			"          \"urls\": [\n" + 
			"            \"https://s3.amazonaws.com/snyk-rules-pre-repository/snapshots/master/patches/npm/ms/20170412/ms_071.patch\"\n" + 
			"          ],\n" + 
			"          \"version\": \"=0.7.1\"\n" + 
			"        }\n" + 
			"      ],\n" + 
			"      \"publicationTime\": \"2017-05-15T06:02:45Z\",\n" + 
			"      \"references\": [\n" + 
			"        {\n" + 
			"          \"title\": \"GitHub Commit\",\n" + 
			"          \"url\": \"https://github.com/zeit/ms/pull/89/commits/305f2ddcd4eff7cc7c518aca6bb2b2d2daad8fef\"\n" + 
			"        },\n" + 
			"        {\n" + 
			"          \"title\": \"GitHub PR\",\n" + 
			"          \"url\": \"https://github.com/zeit/ms/pull/89\"\n" + 
			"        }\n" + 
			"      ],\n" + 
			"      \"semver\": {\n" + 
			"        \"vulnerable\": [\n" + 
			"          \"<2.0.0\"\n" + 
			"        ]\n" + 
			"      },\n" + 
			"      \"severity\": \"low\",\n" + 
			"      \"title\": \"Regular Expression Denial of Service (ReDoS)\",\n" + 
			"      \"from\": [\n" + 
			"        \"goof@1.0.1\",\n" + 
			"        \"express@4.12.4\",\n" + 
			"        \"finalhandler@0.3.6\",\n" + 
			"        \"debug@2.2.0\",\n" + 
			"        \"ms@0.7.1\"\n" + 
			"      ],\n" + 
			"      \"upgradePath\": [\n" + 
			"        false,\n" + 
			"        \"express@4.15.0\",\n" + 
			"        \"finalhandler@1.0.3\",\n" + 
			"        \"debug@2.6.7\",\n" + 
			"        \"ms@2.0.0\"\n" + 
			"      ],\n" + 
			"      \"isUpgradable\": true,\n" + 
			"      \"isPatchable\": true,\n" + 
			"      \"name\": \"ms\",\n" + 
			"      \"version\": \"0.7.1\"\n" + 
			"    }"
			+ "],\n" + 
			"  \"dependencyCount\": 446,\n" + 
			"  \"org\": \"bmvermeer\",\n" + 
			"  \"licensesPolicy\": null,\n" + 
			"  \"isPrivate\": true,\n" + 
			"  \"packageManager\": \"npm\",\n" + 
			"  \"policy\": \"# Snyk (https://snyk.io) policy file, patches or ignores known vulnerabilities.\\nversion: v1.13.3\\nignore: {}\\npatch: {}\\n\",\n" + 
			"  \"ignoreSettings\": null,\n" + 
			"  \"summary\": \"100 vulnerable dependency paths\",\n" + 
			"  \"filesystemPolicy\": false,\n" + 
			"  \"filtered\": {\n" + 
			"    \"ignore\": [],\n" + 
			"    \"patch\": []\n" + 
			"  },\n" + 
			"  \"uniqueCount\": 42,\n" + 
			"  \"path\": \"/Users/brianvermeer/demo/snyk/snyk-demo-todo\"\n" + 
			"}"; 
			
			
		

}
