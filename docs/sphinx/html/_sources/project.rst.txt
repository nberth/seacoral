.. _project:

Project management
==================

The set of analyzed files is called in Seacoral a **project**. Each time you
invoke seacoral with a given configuration, it will either create a new project
or use an existing one, and start a new **run**.

More prcisely, the configuration options either are *project-wide* or *runtime*.
Running seacoral on configuration that only differ in their *runtime* options
will not create a new project: it will re-use the previously generated results
for the new run. If only one project-wide option is changed, then seacoral will
create another project and restart from scratch.

Your projects
-------------

Each invocation of seacoral will generate several directories in the output
path (by default, the directory is `_sc` and it is created where you launch
seacoral). Let's take the following commands as an example:

  .. code-block::

    $ seacoral --config test/c-tests/get_sign/config.toml
    ...
    $ ls _sc/
    6e2af8e901752bf68742591696e757bb  get_sign.c-CC-@1
    get_sign.c-CC-2025-04-08-13:44:21  get_sign.c-CC-last
    last  resources

We see 6 directories for the same project.

1. `6e2af8e901752bf68742591696e757bb/`
   
   This directory name corresponds to the project hash, i.e. the hash of the
   project-wide configuration options used (including the C source code of the project).

2. `get_sign.c-CC-@1`
   
   The directory of the first invocation of seacoral on a set of files and a
   criterion. If you restart seacoral with the same file and coverage
   criterion, it will generate a `get_sign.c-CC-@2` directory.

3. `get_sign.c-CC-2025-04-08-13:44:21`
   
   An alias of the previous directory, but with a timestamp.

4. `get_sign.c-CC-last`
   
   A alias of the first directory, corresponds to the latest run of seacoral
   on a given project.

5. `last`
   
   An alias to the latest seacoral run.

6. `resources/`
   
   A special directory containing the stubs and drivers required for every project.


Project layout
--------------

Let's now see what is generated for a given project:

  .. code-block::

    $ ls _sc/last/
    1.stat   corpus       klee      logs     luncov  testcases
    config   covinfo.csv  labeling  lreplay  store

1. `1.stat`
   
   A file with several statistics on the first run of the project. On a second
   run, the directory will have both the files `1.stat` and `2.stat`.

2. `corpus/`
   
   The directory where the seacoral internal representation of the generated
   tests are stored.

3. `klee/`, `luncov/`, `lreplay/`
   
   The working directory of the testing tools. The current configuration
   only spefified to use these three tools only, but had we added `cbmc` we would
   have had a `cbmc/` directory.

4. `logs/`
   
   The directory in which all the seacoral logs are stored.

5. `testcases/`
   
   The directory where the C files corresponding to the generated tests are
   stored.

6. `config/`
   
   For each run, this directory will contain three different files:
   * `'n'-given.toml`: the toml given for the `n`th run;
   * `'n'-cmdline.txt`: the command line of the `n`th run;
   * `'n'-actual.toml`: the configuration of the `n`th run, merging the given
   configuration file and the command line.

7. `covinfo.csv`
   
   A csv with information about each run (run id, time,
   covered/uncoverable/unknown labels, num of tests generated)

8. `labeling/`
   
   The input files are labelized by `frama-c-lannotate` tool before being sent
   to the orchestrated tools. This directory contains the labelized file as
   well as several internal files used by the `ltest` tools.

9. `store/`
   
   Contains the project runtime store and stubs that allows dynamic tools to share
   their results efficiently. This is only used by seacoral internally.
