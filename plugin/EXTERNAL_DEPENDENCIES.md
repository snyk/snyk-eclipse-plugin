# How to add an external dependency

- need to be referenced in build.properties for the binary buildso they are included in the build
- need to be referenced in MANIFEST.md for the classpath so they are available at runtime
- need to be added to pom.xml as dependency, so the jar can be copied over.
- double click in Eclipse on the plugin.xml, and click on "Update the classpath settings" in the Overview tab.