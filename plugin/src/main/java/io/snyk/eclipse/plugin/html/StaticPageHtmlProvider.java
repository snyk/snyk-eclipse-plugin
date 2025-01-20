package io.snyk.eclipse.plugin.html;

import org.eclipse.core.runtime.Platform;
import org.osgi.framework.Bundle;

import io.snyk.eclipse.plugin.utils.ResourceUtils;

public class StaticPageHtmlProvider extends BaseHtmlProvider {
	private static StaticPageHtmlProvider instance = new StaticPageHtmlProvider();

	public static StaticPageHtmlProvider getInstance() {
		synchronized (StaticPageHtmlProvider.class) {
			if (instance == null) {
				if (instance == null) {
					instance = new StaticPageHtmlProvider();
				}
			}
		}
		return instance;
	}

	private String head = """
				<head>
				    <meta charset="UTF-8">
				    <meta name="viewport" content="width=device-width, initial-scale=1.0">
				    <title>Snyk for Eclipse</title>
				    <style>
				        html {
				            height:100%;
				        }
				        body {
				            font-family: var(--default-font);
				            background-color: var(--background-color);
				            color: var(--text-color);
				            display: flex;
				            align-items: center;
				            justify-content: center;
				            height: 100%;
				        }
				        .container {
				            display: flex;
				            align-items: center;
				            justify-content: center;
				        }
				        .status {
				            text-align: center;
				        }
				        .welcome-text {
				            width: 520px;
				        }
				        .agreement-text {
				            font-size: smaller;
				            display: inline-block;
				            width: 350px;
				        }
				        .logo {
				            margin-right: 20px;
				        }
				        a {
				            color: var(--link-color);
				        }
				        div {
				            padding: 20px;
				        }
				        button {
				            text-align: center;
				            text-decoration: none;
				            background-color: var(--button-color);
				            display: inline-block;
				            border-color: var(--border-color);
				            border-style: solid;
				            border-radius: 5px;
				            font-family: inherit;
				            font-size: inherit;
				            color: inherit;
				        }
				    </style>
				</head>
			""";

	public String getScanningHtml() {
		var html = """
				<!DOCTYPE html>
				<html lang="en">
				%s
				<body>
				    <div class="container">
				        <p>
				            Scanning project for vulnerabilities...<br/>
				            <a class="stop_scan" onclick="window.stopScan();">Stop Scanning</a>
				        </p>
				    </div>
				</body>
				</html>
				""".formatted(head);
		return replaceCssVariables(html);
	}

	public String getDefaultHtml() {
		var html = """
				<!DOCTYPE html>
				<html lang="en">
				%s
				<body>
				    <div class="container">
				        <p>
				            Select an issue and start improving your project.
				        </p>
				    </div>
				</body>
				</html>
				""".formatted(head);
		return replaceCssVariables(html);
	}

	public String getInitHtml() {
		String snykWarningText = Platform.getResourceString(Platform.getBundle("io.snyk.eclipse.plugin"),
				"%snyk.panel.auth.trust.warning.text");

		Bundle bundle = Platform.getBundle("io.snyk.eclipse.plugin");
		String base64Image = ResourceUtils.getBase64Image(bundle, "logo_snyk.png");

		var html = """
				<!DOCTYPE html>
				<html lang="en">
				%s
				<body>
				    <div class="container">
				        <img src='data:image/png;base64,%s' alt='Snyk Logo'>
				        <div class="welcome-text">
				            <p><strong>Welcome to Snyk for Eclipse!</strong></p>
				            <ol>
				                <li align="left">Authenticate to Snyk.io</li>
				                <li align="left">Analyze code for issues and vulnerabilities</li>
				                <li align="left">Improve your code and upgrade dependencies</li>
				            </ol>
				            <p>%s</p>
				            <button type="button" onclick="window.initiateLogin()">Trust project and scan</button>
				            <p class="agreement-text">
				                By connecting your account with Snyk, you agree to
				                the Snyk <a href="https://snyk.io/policies/privacy/">Privacy Policy</a>,
				                and the Snyk <a href="https://snyk.io/policies/terms-of-service/">Terms of Service</a>.
				            </p>
				        </div>
				    </div>
				</body>
				</html>
				""".formatted(head, base64Image, snykWarningText);
		return replaceCssVariables(html);
	}

	//TODO update this when we got new design from Andy
	public String getSummaryInitHtml() {
		var html = """
				<!DOCTYPE html>
				<html lang="en">
				<body>
				    <div class="container">
				        <h3 class="summary-header">SUMMARY</h3>
				                  <button type="button" onclick="window.showAllIssuesTab()">All issues</button>
				                  <button type="button" onclick="window.showDeltaIssuesTab()">Delta issues</button>
				        <div class="summary-row">
				            <span class="icon">⚠️</span>
				            <span class="text">312 issues found in your project</span>
				        </div>
				        <div class="summary-row">
				            <span class="icon">⚡</span>
				            <span class="text">183 issues are fixable</span>
				        </div>

				    </div>
				</body>
				</html>
				""".formatted(head);
		return replaceCssVariables(html);
	}
				"""
				.formatted(head);
		return replaceCssVariables(html);
	}

	public String getFormattedSummaryHtml(String summary) {
		var html = summary
				.formatted(head);
		return replaceCssVariables(html);
	}
}
