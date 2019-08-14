#!/bin/bash
if [ ! -d "snyk_binaries" ]
then
	mkdir "snyk_binaries"
fi

cd snyk_binaries

echo "searching for latest version of snyk CLI"

version=$(curl -s https://api.github.com/repos/snyk/snyk/releases/latest | grep "tag_name"| cut -d : -f2 | tr -d \" | tr -d , | tr -d ' ')
echo "version: $version" 

if [ ! -d "$version" ] 
then
	echo "create dir $version and start download"
    mkdir "$version"
	cd "$version"

	curl -s https://api.github.com/repos/snyk/snyk/releases/latest | grep "browser_download_url.*snyk-macos\"" | cut -d : -f 2,3 | tr -d \" | wget -qi -
	curl -s https://api.github.com/repos/snyk/snyk/releases/latest | grep "browser_download_url.*snyk-linux\"" | cut -d : -f 2,3 | tr -d \" | wget -qi -
	curl -s https://api.github.com/repos/snyk/snyk/releases/latest | grep "browser_download_url.*snyk-win.exe\"" | cut -d : -f 2,3 | tr -d \" | wget -qi -

	echo "download $version complete"
	
	echo "copy latest version to project"

	cp snyk-linux ../../io.snyk.bin.linux/
	cp snyk-macos ../../io.snyk.bin.macos/
	cp snyk-win.exe ../../io.snyk.bin.win/
	
	echo "copy complete"
	
	
else
	echo "dir $version already exists, skip downloading this version"
fi

echo "end"

