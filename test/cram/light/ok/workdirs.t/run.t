  $ seacoral --workdir /bin/somewhere-we-should-not-be-able-to-write
  Error while creating `/bin/somewhere-we-should-not-be-able-to-write': Permission denied
  [124]
  $ seacoral --workdir /bin/"somewhere we should not be able to write even with spaces"
  Error while creating `/bin/somewhere we should not be able to write even with spaces': Permission denied
  [124]
