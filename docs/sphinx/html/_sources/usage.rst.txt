.. _usage:

Usage
=====

Simple example
--------------

.. TODO: redo the logs to match the current version of the tool

Workflow
~~~~~~~~

Consider the following example with a single condition.

.. code-block:: c
   :caption: get_sign.c

   int get_sign(int i) {
     if (i < 0) return -1;
     return 1;
   }


This function has two branches: either `i` is strictly negative or `i` is positive.
Our goal is to generate a test case for each of these branches, proving all
the code is reachable. SeaCoral will work as follows.

1. First, it will call `LTest <https://micdel.fr/ltest.html>`_ for annotating the
   C code with `predicate coverage labels`, or `pc_label` s.
   We are not talking here about usual C labels; `pc_label` s are defined as source
   code location associated to a C boolean expression. A label is said `reachable`
   if there exist a function input for which the label location is reached and the
   boolean expression evaluated to a non null integer.

  .. code-block:: c
     :caption: get_sign_label.c

     int get_sign(int i) {
       int __retres;
       pc_label(i < 0,1);
       pc_label(! (i < 0),2);
       if (i < 0) {
         __retres = -1;
         goto return_label;
        }
       __retres = 1;
       return_label: return __retres;
     }

  We have two `pc_label` s, one for each function branch: `i < 0` and `i ≥ 0`.

2. Then, `sc` calls a given set of tools to analyze the labelized C code.
   The tools orchestrated by `sc` are `klee`, `libfuzzer`, `cbmc` and `luncov`.
   Each of them create a custom harness where each `pc_label` correspond to specific
   instructions built from tool internals.

3. Once each tool finished their analysis, they provide the test cases they
   found for covering the function branches and produce a coverage report.

Using SeaCoral
~~~~~~~~~~~~~~

Running `seacoral` on this file with `klee` will automatically start the previous workflow:

  .. code-block::

    $ seacoral --files get_sign.c --entrypoint get_sign --tools klee
    {Sc_main.Init} Starting to log into `_sc/get_sign.c-CC-@1/logs/1.log`
    {Sc} Initializing working environment
    {Sc} Preprocessing `get_sign' (get_sign.c)
    {Sc} Doing the hard work...
    {Sc} Launching klee on `get_sign' (get_sign.c)
    {Sc} Updating static database with new inputs...
    {Sc} Hard work done
    {Sc} Coverage statistics for `get_sign' (get_sign.c): cov: 2 (100.0%) uncov: 0 (0.0%) unkwn: 0 (0.0%) with 2 tests
    {Sc} Covered labels: {1, 2}

The generated tests can be found in `_sc/get_sign.c-CC-get_sign-last/testcases_CC`.
In this case, we get three files:

.. code-block:: c
   :caption: 0001-klee.c

     int main (){
       // Globals (if any)
       
       // Effective argument(s)
       int  i  = -1;
       get_sign (i);
       return 0;
     }

.. code-block:: c
   :caption: 0002-klee.c

     int main (){
       // Globals (if any)
       
       // Effective argument(s)
       int  i  = 0;
       get_sign (i);
       return 0;
     }

The first test reaches the first `pc_label` (`i < 0`) while the second
one reaches the second `pc_label` (`i ≥ 0`).


Using multiple tools
--------------------

This section presents the usage of `sc` on the `tritype` example, checking
the property of a triangle.

.. code-block:: c
  :caption: tritype.c

  /* Should return the type of the triangle
     which has sides of these lengths.
     3 = not a triangle
     2 = equilateral triangle
     1 = isoceles triangle
     0 = other triangle
  */

  struct triangle {
    int i;
    int j;
    int k;
  };

  int tritype(struct triangle t){
  	int type_code;
  	int i = t.i;
  	int j = t.j;
  	int k = t.k;
  	if ((i == 0) || (j == 0) || (k == 0)) type_code = 4;
  	else {
  		type_code = 0;
  		if (i == j) type_code = type_code + 1;
  		if (i == k) type_code = type_code + 2;
  		if (j == k) type_code = type_code + 3;
  		if (type_code == 0){
  			if ((i+j <= k) || (j+k <= i) || (i+k <= j))
  				type_code = 4;
  			else
  				type_code = 1;
  		}
  		else if (type_code > 3) type_code = 3;
  		else if ((type_code == 1) && (i+j > k)) type_code = 2;
  		else if ((type_code == 2) && (i+k > j)) type_code = 2;
  		else if ((type_code == 3) && (j+k > i)) type_code = 2;
  		else if (type_code > 10) type_code = -1;
  		else type_code = 4;
  	}
  	return type_code;
  }

The label annotation step (1.) returns 36 labels, one for each boolean
expression in conditions and its negation. One of these labels is actually
unreachable: the last condition `type_code > 10` is actually impossible.  If we
start SeaCoral with `klee`, `libfuzzer` or `cbmc`, only 35 labels out of 36 will be
covered:

.. code-block::

   $ seacoral test/c-tests/tritype_uncov/tritype.c --entrypoints tritype --tools cbmc
   {Sc_main.Init} Starting to log into `_sc/tritype.c-CC-@1/logs/1.log`
   {Sc} Initializing working environment
   {Sc} Preprocessing for _sc/tritype.c-CC-tritype-@1/tritype.c
   {Sc} Preprocessing `tritype' (tritype.c)
   {Sc} Doing the hard work...
   {Sc} Launching cbmc on `tritype' (tritype.c)
   {Sc} Updating static database with new inputs...
   {Sc} Hard work done
   {Sc} Coverage statistics for `tritype' (tritype.c): 35/36 (97.2%)

SeaCoral saved the result of this analysis in `_sc/tritype.c-CC-tritype-last` —
which is a symbolic link equivalent to `_sc/tritype.c-CC-tritype-@1` in the
above log.  The status of the project will be re-used by the next analyses of
`tritype`. We can now use one of the tools that allows to discover uncoverable
labels in the code: `cbmc-assert`, `cbmc-clabel` or `luncov`.

.. code-block::

   $ seacoral --files tritype.c --entrypoints tritype --tools cbmc-assert
   {Sc_main.Init} Starting to log into `_sc/tritype.c-CC-@2/logs/2.log`
   {Sc} Initializing working environment
   {Sc} Preprocessing for _sc/tritype.c-CC-tritype-@2/tritype.c
   {Sc} Preprocessing `tritype' (tritype.c)
   {Sc} Doing the hard work...
   {Sc} Launching cbmc-assert on `tritype' (tritype.c)
   {Sc} Updating static database with new inputs...
   {Sc} Hard work done
   {Sc} Coverage statistics for `tritype' (tritype.c): 35/36 (97.2%) +1 uncov.

We see that `cbmc-assert` discovered a new uncoverable label, corresponding to
the impossible condition.

NB: tools can be used in parallel by giving a list of tools (separated by a
comma) to the `--tools` option.

Annotations
-----------

Here is one of the tests generated by cbmc:


.. code-block:: c
   :caption: 0001-cbmc.c

   int main (){
     struct triangle = {.i = 2147262475, .j = 2147262475, .k = -32746};
     tritype (i,j,k);
     return 0;
   }

While this is a perfectly valid test, the negative value of `k` is disapointing
(as we are working with triangles). We definitely should have used `unsigned int` instead of
`int` in our triangle structure definition.

But let's say there is an invariant forcing these values to be strictly positive
in the project using the `tritype` function. We want to generate tests that only
include triangles with positive values. SeaCoral allows to annotate the function with
the instruction `sc_assume(p)`, where `p` is a boolean expression.

.. code-block:: c
  :caption: tritype_annoted.c

  #include<sc_annots.h>

  ...

  int tritype(struct triangle t){
  	int type_code;
  	int i = t.i;
  	int j = t.j;
  	int k = t.k;

	sc_assume(i > 0);
	sc_assume(j > 0);
	sc_assume(k > 0);

	if ((i == 0) || (j == 0) || (k == 0)) type_code = 4;
  	else {
  		type_code = 0;
  		if (i == j) type_code = type_code + 1;
  		if (i == k) type_code = type_code + 2;
  		if (j == k) type_code = type_code + 3;
  		if (type_code == 0){
  			if ((i+j <= k) || (j+k <= i) || (i+k <= j))
  				type_code = 4;
  			else
  				type_code = 1;
  		}
  		else if (type_code > 3) type_code = 3;
  		else if ((type_code == 1) && (i+j > k)) type_code = 2;
  		else if ((type_code == 2) && (i+k > j)) type_code = 2;
  		else if ((type_code == 3) && (j+k > i)) type_code = 2;
  		else if (type_code > 10) type_code = -1;
  		else type_code = 4;
  	}
  	return type_code;
  }

With this version of tritype, all tests will verify the assumed properties.
Note that with these `sc_assume` s, SeaCoral fail to cover some labels:

.. code-block::

   $ seacoral --files tritype_annoted.c --entrypoints tritype --tools cbmc
   {Sc_main.Init} Starting to log into `_sc/tritype.c-CC-@1/logs/1.log`
   {Sc} Initializing working environment
   {Sc} Preprocessing `tritype' (tritype.c)
   {Sc} Doing the hard work...
   {Sc} Launching cbmc on `tritype' (tritype.c)
   {Sc} Updating static database with new inputs...
   {Sc} Hard work done
   {Sc} Coverage statistics for `tritype' (tritype.c): 32/36 (88.9%)

This is because the first condition (`(i == 0) || (j == 0) || (k == 0)`) is
now unsatisfiable. In the current setting, each boolean expression is associated
to a specific label. The three labels  `i == 0`, `j == 0` and `k == 0` are now
unreachable, which can be proven by `cbmc-assert`:

.. code-block::

   $ seacoral tritype_annoted.c --entrypoints tritype --tools cbmc-assert

   {Sc} Initializing working environment
   {Sc} Preprocessing `tritype' (tritype.c)
   {Sc} Doing the hard work...
   {Sc} Launching cbmc on `tritype' (tritype.c)
   {Sc} Updating static database with new inputs...
   {Sc} Hard work done
   {Sc} Coverage statistics for `tritype' (tritype.c): 32/36 (88.9%) +4 uncov.

Note that `sc_assume` can not only be used at the beginning of a function,
but in the middle or at the end as well. Here is the exact semantic of
`reachability` with respect to `sc_assume`: a `pclabel` `l` is reachable if and only if
there exist an input such that `l` is reached and all `sc_assume(p)` triggered during the
execution evaluate `p` as `true`.

NB: it is a bad practice to expect preconditions/invariants on values when performing
unit-testing. Still, in some cases, it is interesting to force some values. A test set
with null pointers leading to errors does not form the best coverage set. By assuming
pointers are not null, we can generate prettier test sets.

Options
-------

seacoral [OPTIONS]

Launches the SeaCoral orchestrator. The following OPTIONS are available:

==========================  ====================================================================
Options                     Documentation
==========================  ==================================================================== 
``-C``                      Uses a :ref:`configuration file<Configuration>`.
``--check-config-only``     Only checks the configuration.
``--clean-start``           Removes the project with the same hash before starting its analysis.
``--criterion``             Coverage criterion.
``--debug``                 Selects the console log level.
``--disable-syntax-check``  Disable check for syntax errors in input files prior to labeling.
``--enable-syntax-check``   Enables check for syntax errors in input files (the default).
``--entrypoint``            Selects the function to analyze.
``--external-libs``         External libraries required to compile the project.
``--files``                 The list of files of the project.
``--force-preprocess``      Forces the preprocessing, even if it already has been performed.
``--functions-to-cover``    Lists the functions to cover.
``-I``                      The list of directories to be searched for header files.
``--init``                  Generates a default configuration file.
``--inputs``                The input files.
``--name``                  The name of the project.
``--show-toml-doc``         Print the configuration documentation and gracefully exit.
``--tools``                 Selects the tools to use for the tests.
``--workdir``               Directory where the working files are stored (default: _sc).
``--help``                  Display the list of options.
==========================  ====================================================================
