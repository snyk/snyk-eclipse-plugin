## Eclipse Plugin Release Steps

**Update Version Numbers**

-   Update the release version in all files that specify the Eclipse plugin version.

-   Do not change values that do not represent the plugin version.


**Build Artifacts**

-   Use  `mvnw clean package`  or  `mvnw clean verify`  to build the project and ensure artifacts are created with the correct versions.

-   Verify the plugin JAR file in the  `target/repository/plugins`  folder to confirm the version is correct.


**Testing**

-   Ensure all tests for the latest commit have passed before proceeding with the release.


**Release via GitHub Actions**

-   Trigger the release workflow in GitHub Actions.

-   Select the appropriate version type (major, minor, patch).

-   If this is a hotfix, select the hotfix branch.


**Release Notes**

-   Edit or generate the release notes on GitHub for this release.


**Install and Verify Snyk Plugin in Eclipse**

-   Open Eclipse, navigate to  `Help`  →  `Eclipse Marketplace`.

-   Search for "Snyk" and install the latest version of the plugin.

-   Go to  `Eclipse`  →  `About Eclipse`, click  `Installation Details`.

-   Search for "Snyk" and verify that the installed version matches the intended release[6](https://www.clear.rice.edu/comp310/Eclipse/installation.html)[12](https://vaadin.com/docs/v8/framework/installing/installing-eclipse).


**Verify Snyk CLI**

-   Ensure the Snyk CLI is installed, set to the  `stable`  release channel, and automatic updates are enabled.

-   Run the CLI binary in the terminal and check the version using:

text

`snyk --version` 

-   Confirm the version matches the latest release (refer to the  `#hammerhead-releases`  channel in Slack for the correct version)[7](https://snyk.io/blog/snyk-cli-cheat-sheet/)[11](https://brightsec.com/blog/snyk-cli-quick-guide-installation-and-common-commands/).


**Manual Scan**

-   Run a manual scan using the latest version of the Eclipse plugin and the latest CLI to verify functionality.


**Marketplace Credentials**

-   Log in to the Eclipse Marketplace using the password from 1Password.

-   Update the plugin listing if necessary.