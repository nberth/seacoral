# SeaCoral: A Collaborative Test Generation Toolset

[![CI][ci-badge]](https://github.com/ocamlpro/seacoral/actions)
[![Release][release-badge]](https://github.com/ocamlpro/seacoral/releases)

SeaCoral is an orchestration tool that is able to combine the power of
a wide range of test generation techniques in order to automatically
produce tests for your C projects.

## Running SeaCoral in a Docker container

Instructions for using SeaCoral in a Docker container are provided [in
the `docker` subdirectory](docker/README.md).

### Evaluation Artifact

An artifact that contains a Docker image, usage instructions, and
examples, is available on [Zenodo][artifact-doi][^artifact-note].

## Resources

* Website: https://ocamlpro.com/seacoral
* Documentation: https://ocamlpro.github.io/seacoral/
* Sources: https://github.com/ocamlpro/seacoral

## Funding notice

Part of this work was supported by the [AID ("Agence de l'innovation
de défense")][AID] through the [RAPID ("Régime d'APpui à l'Innovation
Duale")][RAPID] project AutoCouv: "Génération automatique de tests par
couverture des labels".

---
Copyright © 2024-2026 OCamlPro

[ci-badge]: https://github.com/ocamlpro/seacoral/workflows/CI/badge.svg
[release-badge]: https://img.shields.io/github/release/ocamlpro/seacoral.svg

[AID]: https://www.defense.gouv.fr/aid
[RAPID]: https://www.defense.gouv.fr/aid/deposez-votre-projet/rapid-regime-dappui-linnovation-duale

[artifact-doi]: https://doi.org/10.5281/zenodo.17357287
[^artifact-note]: This artifact provides the material that enables the
    reproduction of every run of SeaCoral that is described in the
    paper "SeaCoral: A Collaborative Test Generation Toolset" by
    Nicolas Berthier, Steven de Oliveira, Nikolai Kosmatov, and
    Delphine Longuet, that was submitted to TACAS 2026.
