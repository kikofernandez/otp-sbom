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

1. `make install`
   This will clone Erlang `maint` branch to analyze and ORT specific branch
   It will also create a docker image with the software necessary to run ORT
2. `make docker-analyze`
3. `make docker-scan`   
