# Contributing

## Configuring the plugin for local development

### Eclipse PDE

In order to get started, you need to install PDE from the eclipse
marketplace.

1. Open Eclipse IDE
2. Go to Help > Install New Software
3. In the install window, select The Eclipse Project Updates
4. In the list, select Eclipse Plugin Development Tools
5. Proceed with the license terms and click Finish.

### Sorting out dependencies

If `org.eclipse.*` dependencies are causing compilation errors, open `/target-platform/target-platform.target`
and click in the top right corner (Reload Target Platform).

![](docs/target-platform.png "Target Platform Libraries")

If external dependencies are not picked up: 
- run `./mvnw package` to fetch all required jars in the repo root, 
- reload Eclipse (F5 in `plugin`)
- and add as external dependencies the jars under `/plugin/target/dependency`

![](docs/add-jars.png "Adding External Dependencies")

### Running the plugin

In order to run Eclipse with the plugin, double-click `plugin.xml`
and click the overview tab. From there you can run a new instance of
eclipse by clicking "Launch an Eclipse application".

