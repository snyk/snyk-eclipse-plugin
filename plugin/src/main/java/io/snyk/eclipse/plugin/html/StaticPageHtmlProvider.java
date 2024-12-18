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

	public String getInitHtml() {
		String snykWarningText = Platform.getResourceString(Platform.getBundle("io.snyk.eclipse.plugin"),
				"%snyk.panel.auth.trust.warning.text");

		Bundle bundle = Platform.getBundle("io.snyk.eclipse.plugin");
		String base64Image = ResourceUtils.getBase64Image(bundle, "logo_snyk.png");

		var html = """
				<!DOCTYPE html>
				<html lang="en">
				<head>
				    <meta charset="UTF-8">
				    <meta name="viewport" content="width=device-width, initial-scale=1.0">
				    <title>Snyk for Eclipse</title>
				    <style>
				        body {
				            font-family: var(--default-font);
				            background-color: var(--background-color);
				            color: var(--text-color);
				        }
				        .container {
				            display: flex;
				            align-items: center;
				        }
				        .welcome-text {
				            width: 530px;
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
				<body>
				    <div class="container">
				        <img src='data:image/png;base64,%s' alt='Snyk Logo'>
				        <div class="welcome-text">
				            <p><strong>Welcome to Snyk for Eclipse</strong></p>
				            <ol>
				                <li align="left">Authenticate to Snyk.io</li>
				                <li align="left">Analyze code for issues and vulnerabilities</li>
				                <li align="left">Improve your code and upgrade dependencies</li>
				            </ol>
				            <p>%s</p>
				            <button type="button" onclick="window.initiateLogin()">Trust project and scan</button>
				            <p>
				                By connecting your account with Snyk, you agree to
				                the Snyk <a href="https://snyk.io/policies/privacy/">Privacy Policy</a>,
				                and the Snyk <a href="https://snyk.io/policies/terms-of-service/">Terms of Service</a>.
				            </p>
				        </div>
				    </div>
				</body>
				</html>
				""".formatted(base64Image, snykWarningText);
		return replaceCssVariables(html);
	}
}
