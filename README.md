# Snyk Eclipse Plugin

This is the repo for the "Snyk Security - Code and Open Source Dependencies" for Eclipse.

## Description

Snyk scans your open source dependencies, detects and helps fix security issues in your project from within your Eclipse IDE -- Woof!

You can use Snyk for free on your Java, Scala, JavaScript .Net, Ruby projects and many more! Each scan tests your application against Snyk's extensive vulnerability database, to can identify and advise you how to fix security problems.

**Snyk detects the critical vulnerability Log4Shell, which was found in the open source Java library log4j-core - a component of one of the most popular Java logging frameworks, Log4J. The vulnerability was categorized as Critical with a CVSS score of 10, and with a mature exploit level.**

## Download

- Manual downloads : https://github.com/snyk/snyk-eclipse-plugin/releases
- Eclipse market place (recommended): https://marketplace.eclipse.org/content/snyk-vuln-scanner

## Supported Eclipse versions

- 2019-09 (4.13)
- 2019-06 (4.12)
- 2019-03 (4.11)
- 2018-12 (4.10)
- 2018-09 (4.9)
- Photon (4.8)
- Oxygen (4.7)

## Supported Operating Systems

- MacOSX
- Linux
- Windows 10

## Configuration

### Environment

To analyse projects, the plugin uses the Snyk CLI which needs some environment variables. The following variables are needed or helpful, dependent on the type of project you analyse:

- `PATH` should contain the path to needed binaries, e.g. to maven, gradle, python, go.
- `JAVA_HOME` should contain the path to the JDK you want to use for analysis of Java dependencies
- `https_proxy` and `http_proxy` if you are using a proxy server. The format is `http_proxy=http://username:password@proxyhost:proxyport`

Setting these variables only in a shell environment via e.g. `~/.bashrc` is not sufficient, if you don't start the Eclipse
from the command line or create a script file that starts it using a shell environment.

- On **Windows**, you can set the variables, using the GUI or on the command line using the `setx` tool.
- On **macOS**, the process `launchd` needs to be aware of the environment variables if you want to launch the IDE from Finder directly. You can set environment variables for applications launched via Finder using the `launchctl setenv` command e.g. on start-up or via a script you launch at user login. The provision of environment variables to the macOS UI is sometimes changing between operating system releases, so it might be easier to create a small shell script that launches the IDE to leverage the shell environment, that can be defined via `~/.bashrc`.
- On **Linux**, updating the file `/etc/environment` can be used to propagate the environment variables to the windows manager and UI.

### Proxy

If you need to use a proxy server to connect to the internet, please configure it using the environment variables.

