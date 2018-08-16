# Install

## Binary Installation

git-cdflow provides 64-bit installation for GNU/Linux and Mac OS X.

1. Download the tar.gz file for your platform from the [release page](https://github.com/Etiqa/git-cdflow/releases)
2. Extract the archive
```bash
tar xvzf git-cdflow.tar.gz
```
3. `cd` to the directory you extracted the archive, for example `cd /Users/johndoe/git-cdflow`
4. `cd` to the bin subdirectory `cd bin`
5. Test that the toolkit is functional
```bash
./git-cdflow version
```
6. Append the git-cdflow bin directory to your `PATH` environment variable. There are different ways to do it, for example adding the following row to your `~/.bashrc` file
```bash
export PATH=$PATH:/Users/johndoe/git-cdflow/bin
```
7. Verify that the binaries will be found by the shell:
```bash
source ~/.bashrc
which git-cdflow
```
8. Your configuration should be done and git-cdflow should be recognized as a git plugin, so you can run it without the dash:
```bash
git cdflow help
```