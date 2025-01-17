<!--
SPDX-FileCopyrightText: 2025 Erlang/OTP and its contributors

SPDX-License-Identifier: Apache-2.0
-->

# SBOM Creation

To create a SBOM, the following are prerequisites

- Erlang
- `make`
- `git`
- Docker (must be available to create an image with the SBOM software requirements)

# Instructions

1. `make sbom`
   This will clone Erlang/OTP `maint` branch, and create its source SBOM using ORT (oss-review-toolkit).

# Extra

There is an escript to patch some of the source SBOM produced by ORT.

The script can be run as:

``` shell
make fix-sbom
```

The script contains also options useful to get an overview and
easily produce mappings from licenses to files.

## Extra: REUSE Compliance

Requirements: Install `reuse` (https://github.com/fsfe/reuse-tool).

The escript is also able to produce a `REUSE.toml` file
that makes Erlang/OTP compliant with REUSE tools. The only drawback
is that we need to create our own `LicenseRef-NONE` to write the
`NONE` license (unknown) to all unknown files.

``` shell
cp otp/scan-result.json .
./otp_compliance.escript explore classify-license
./otp_compliance.escript explore classify-license-copyright --output-file license_copyright.txt
../otp_compliance.escript explore reuse-gen-toml --input-file license_copyright.txt > otp/REUSE.toml
cd otp
reuse download --all
reuse lint
```

TODO: There are some minor issues with unicode, and one specific file in OTP will crash `reuse`.
