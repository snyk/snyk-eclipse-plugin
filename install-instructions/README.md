# Snyk Security - Code and Open Source Dependencies - beta

Supported for Eclipse on **_MacOS_** and **_Linux_**

Support for **_Windows_** is still in progress.

## Installation

- unpack the zipfile
- open eclipse
- Go to **Help** -> **Install new software**
- **Add** -> **Local** -> _browse to the unpacked zip_ -> **Open** -> **Add**
- Check the _Snyk Security - Code and Open Source Dependencies_ and follow the rest of the steps in the wizard.
- Eclipse might need to restart

## Configuration

- In the top menu bar click **Window** -> **Show View** -> **Other**
- A Window will pop up and you choose **Snyk** -> **Snyk View**
- In the _Snyk View_, open the context menu (right click) -> **Preferences**
- Put in you Snyk Auth Token
  ,this can be found in your account settings on snyk.io (also known as API token)
- In path fill in the path of you package managers (e.g. Maven) if they are not installed on the default location

## Usage

- With the **PLAY** button in the top right of the _Snyk View_ you can run the scan over all projects.
- NOTE: this scan runs in the background and might take a while
- NOTE: the **STOP** button sends a message to the background process to stop. This can also take some time before processed
- For rescan
  - click play again to scan the whole workspace
  - right click on specific project in the _Snyk View_ to scan selectively

## Uninstall

- Go to **About Eclipse** -> **Installation Details** to uninstall any plugin / feature
