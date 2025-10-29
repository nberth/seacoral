# Using Seacoral in a Docker image

## Retrieving the development image from OCamlPro's registry

You first need access to the registry to be able to retrieve the
image.  To do so, login to https://gitlab.ocamlpro.com and navigate to
https://gitlab.ocamlpro.com/-/user_settings/personal_access_tokens.
Create a new token with `read_registry` scope.

Then, login to the registry using the following command (replace
`<token>` with the token you created on gitlab):
```shell
echo "<token>" | docker login registry.ocamlpro.com -u oauth2accesstoken --password-stdin
```

After this, the latest development image can be retrieved using
```shell
docker pull registry.ocamlpro.com/ocamlpro/seacoral:latest
```
or used directly with
```shell
docker run --rm -it registry.ocamlpro.com/ocamlpro/seacoral:latest
```

The latter command starts a shell in a new container running the
image; `seacoral` is available as a command-line utility while in this
shell.

The default working directory in containers derived from the image is
`/workdir`.  You can pass the options `--volume .:/workdir` to bind
the current working directory to `/workdir` in the container.

**Warning**: `seacoral` won't be in the `PATH` if a custom shell is
specified at the end of the `docker run` command.  In this case, you
can run `eval $(opam env)` (or equivalent) to update the environment
accordingly.
