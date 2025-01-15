# SPDX-FileCopyrightText: 2025 Erlang/OTP and its contributors
#
# SPDX-License-Identifier: Apache-2.0

##
# Project Title
#
# @file
# @version 0.1


all: sbom
clean:
	@-rm -rf otp ort otp/scan-result.json otp/bom.spdx.json bom.spdx.json

# .PHONY: fix-sbom docker-build analyze scan report

ort:
	git clone -b kiko/erlang-sbom https://github.com/kikofernandez/ort.git

otp:
	git clone -b maint https://github.com/erlang/otp.git

docker-build: otp ort
	docker build --tag sbom \
            --file "Dockerfile" \
            . \
			--network=host

# Run outside Docker
fix-sbom:
	./otp_compliance.escript sbom otp-info --sbom-file otp/bom.spdx.json --input-file otp/scan-result.json
	cp otp/bom.spdx.json . # Patched source SBOM

#
# Run Docker commands
#
sbom:
	make docker-build
	make docker-run
	make fix-sbom

docker-run:
	docker run --rm -v $(PWD):/github \
               --network host -u $(id -u ${USER}):$(id -g ${USER}) \
               -w /github \
               sbom /bin/sh -c "make analyze && make scan && make report"

# Commands to run inside the docker container
analyze:
	cd ort && \
	./gradlew cli:run --args="-c /github/config.yml analyze -i /github/otp -o . -f JSON --repository-configuration-file=/github/.ort.yml"

scan:
	cd ort && \
	./gradlew cli:run --args="-c /github/config.yml scan -o /github/otp -f JSON -i /github/ort/cli/analyzer-result.json"

report:
	cd ort && \
	./gradlew cli:run --args="report -i /github/otp/scan-result.json -o /github/otp -f SpdxDocument -O SpdxDocument=outputFileFormats=JSON" && \
	./gradlew cli:run --args="report -i /github/otp/scan-result.json -o /github/otp -f PlainTextTemplate -O PlainTextTemplate=template.path=/github/COPYRIGHT.md.ftl"

# end
