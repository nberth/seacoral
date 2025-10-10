# Welcome!

Welcome in Seacoral's Docker image, where Seacoral and its runtime
dependencies have all been pre-installed for you.

## Availability check

You may type `seacoral --version` to check which version of the
Seacoral command-line utility is installed.

## Setting up an empty project

You may use
```shell
mkdir project-dir
cd project-dir
seacoral config initialize
```
to setup an empty project in a directory `project-dir`.

## Launching Seacoral on a pre-configured project

Assuming an already configured Seacoral project in `project-dir`
(i.e. there exists a configuration file `project-dir/seacoral.toml`),
you may just start directly with the main test-generation command with
```shell
cd project-dir
seacoral
```
or alternatively
```shell
seacoral --config project-dir/seacoral.toml
```

*Note*: with the default configuration, Seacoral's output will be
placed in a directory `_sc` located within the current working
directory.
