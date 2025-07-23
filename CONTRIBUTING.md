# Contributing to the Snyk IDE Extensions

We welcome contributions, but please read first! To ensure a smooth process and that your valuable work aligns with our roadmap, please keep the following in mind to help manage expectations:

## 1. Planning your changes

Before undertaking any changes or new features, please discuss your plans with us. This helps align on scope, design, technical approach, and priority.  
Even bug fixes can have unforeseen impacts or alternative solutions better suited for the codebase, so please ask first, we will be happy to discuss.  
Please raise a request with [support](https://support.snyk.io). (Snyk employees, use `#ask-ide`)

## 2. Where changes should be made

Consider whether your proposed change should be implemented within the IDE extension(s) or in the shared Language Server and related stack.
- [Snyk Language Server](https://github.com/snyk/snyk-ls)
- [Go Application Framework](https://github.com/snyk/go-application-framework)
- [Code Client Go](https://github.com/snyk/code-client-go)

## 3. Cross-IDE consistency

If your change is applicable to other Snyk IDE plugins as well, we may expect you to submit similar PRs for the other relevant IDE repositories after your initial PR has been reviewed and approved, as they will _usually_ need to be merged all at once or not at all.
- [Snyk VSCode extension](https://github.com/snyk/vscode-extension)
- [Snyk IntelliJ plugin](https://github.com/snyk/snyk-intellij-plugin)
- [Snyk Visual Studio extension](https://github.com/snyk/snyk-visual-studio-plugin)

## 4. Manual testing

All changes must be thoroughly manually tested by you.  
For visual changes the PR template asks for screenshots, so this is a good opportunity to snap them.

## 5. Documentation changes

Any user-facing changes will require [documentation](https://docs.snyk.io/) changes, which you will need to prepare.
If you do not have access to our content management system (you are not a Snyk employee), please add the documentation changes required (including new wording and screenshots) to the PR description.

We can instruct you on what to add to the CHANGELOG.md, so please ask.

---

# Making changes

## Configuring the plugin for local development

### Eclipse PDE

In order to get started, you need to install PDE from the eclipse
marketplace.

1. Open Eclipse IDE
2. Go to Help > Install New Software
3. In the install window, select The Eclipse Project Updates
4. In the list, select Eclipse Plugin Development Tools
5. Proceed with the license terms and click Finish.

### Importing the project

If you import the parent project as a maven project, eclipse should take care of everything.

![](docs/import-maven.png "Import maven project")

If you encounter some issues with dependencies keep on reading.

#### Sorting out dependencies

If `org.eclipse.*` dependencies are causing compilation errors, open `/target-platform/target-platform.target` and click in the top right corner (Reload Target Platform).

![](docs/target-platform.png "Target Platform Libraries")

If external dependencies are not picked up:
- run `./mvnw package` to fetch all required jars in the repo root,
- reload Eclipse (F5 in `plugin`)
- and add as external dependencies the jars under `/plugin/target/dependency`

![](docs/add-jars.png "Adding External Dependencies")

#### Fix compilation errors
Sometimes Java build path is incorrectly set when you import project into Eclipse.

1. Make sure you have source folder set to `src/main/java` for `io.snyk.eclipse.plugin` and `src/test/java` for `io.snyk.eclipse.plugin.tests`.

    Navigate to `Project > Properties > Java Build Path > Source` and make sure the correct folder is selected for both projects.

![](docs/source-path.png "Java Build Path: Source Folder")

2. Make sure "Plug-in dependencies" and "Maven managed dependencies" are also on the Classpath.

    Navigate to `Project > Properties > Java Build Path > Libraries`. Select "Classpath" and "Add Library...", then select "Plug-in dependencies" and "Finish". Perform the same steps for "Maven managed dependencies".


### Running the plugin

In order to run Eclipse with the plugin, double-click `plugin.xml`
and click the overview tab. From there you can run a new instance of
eclipse by clicking "Launch an Eclipse application".

