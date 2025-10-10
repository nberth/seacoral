Abstract the commands that may appear in tarces (and be sure to use absolute paths):
  $ test "x$CLANG" = "x" && CLANG=clang || true
  $ export CLANG="$(command -v "$CLANG" 2&>/dev/null)"
  $ export BUILD_PATH_PREFIX_MAP="\$CLANG=$CLANG:$BUILD_PATH_PREFIX_MAP"
  $ unset CPPFLAGS LDFLAGS

Syntax errors in given C files are reported during intialization:
  $ seacoral --config invalid-c-file.toml
  [A]{Sc} Starting to log into `_sc/invalid.c-CC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [E]{Sc} Syntax errors detected in codebase:
          $TESTCASE_ROOT/./invalid.c:1:16: error: expected ')'
          int foo (int x y) {
                         ^
          $TESTCASE_ROOT/./invalid.c:1:9: note: to match this '('
          int foo (int x y) {
                  ^
          1 error generated.
  [124]

Even better, `clang`'s output is not repeated in logs.
  $ seacoral --config invalid-c-file.toml --console-level 5
  [D]{Sc} Salt: 0befa988993d2a49e8361cd2a4d2e0ca
  [D]{Sc} Previous runs: 1
  [A]{Sc} Starting to log into `_sc/invalid.c-CC-@2/logs/2.log'
  [I]{Sc} Configuration loaded from `invalid-c-file.toml'
  [A]{Sc} Initializing working environment...
  [D]{Sc_project} Initializing
  [I]{Sc_project} Gathering the codebase...
  [I]{Sc_project} Syntax-checking the codebase...
  [D]{Sc_sys} /bin/sh -c "$CLANG -g -include $TESTCASE_ROOT/_sc/shared/include/noop-labels.h -I'$TESTCASE_ROOT/_sc/shared/include' -c _sc/invalid.c-CC-@2/labeling/invalid.c -fsyntax-only -fcolor-diagnostics"
  [D]{(clang)} $TESTCASE_ROOT/./invalid.c:1:16: error: expected ')'
  [D]{(clang)} int foo (int x y) {
  [D]{(clang)}                ^
  [D]{(clang)} $TESTCASE_ROOT/./invalid.c:1:9: note: to match this '('
  [D]{(clang)} int foo (int x y) {
  [D]{(clang)}         ^
  [D]{(clang)} 1 error generated.
  [D]{Sc_sys} /bin/sh -c "$CLANG -g -include $TESTCASE_ROOT/_sc/shared/include/noop-labels.h -I'$TESTCASE_ROOT/_sc/shared/include' -c _sc/invalid.c-CC-@2/labeling/invalid.c -fsyntax-only -fcolor-diagnostics" terminated with status EXITED(1)
  [E]{Sc} Syntax errors detected in codebase:
          (see log output above for details)
  [124]
