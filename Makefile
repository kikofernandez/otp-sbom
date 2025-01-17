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
	docker run --rm -u $(shell id -u):$(shell id -g) \
               -v $(PWD):/github \
               --network host  \
               -w /github \
               sbom /bin/sh -c "export GRADLE_USER_HOME=/github/.gradle && \
                                export HOME=/github && \
                                export ORT_CONFIG_DIR=/github/.ort/config && \
                                export ORT_DATA_DIR=/github/.ort && \
                                make analyze && make scan && make report"

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

test-install:
	# sudo apt-get update && sudo apt-get -y upgrade

	sudo -E apt-get install -y wget apt-transport-https gnupg
    # wget -O - https://packages.adoptium.net/artifactory/api/gpg/key/public | sudo apt-key add - && \
    # echo "deb https://packages.adoptium.net/artifactory/deb $(awk -F= '/^VERSION_CODENAME/{print$2}' /etc/os-release) main" | tee /etc/apt/sources.list.d/adoptium.list && \
    # sudo -E apt-get update && \
    # sudo -E apt-get install -y temurin-21-jdk
	sudo -E apt-get install -y build-essential git python3 python3-pip yarn nodejs

	pip install --upgrade pip setuptools wheel && pip install scancode-toolkit

	sudo -E apt-get install locales && sed -i 's@# en_US.UTF-8@en_US.UTF-8@g' /etc/locale.gen && locale-gen && \
	update-locale LANG=en_US.UTF-8
	# ENV LC_ALL en_US.UTF-8

test: ort
	export GRADLE_USER_HOME=.gradle && \
    export HOME=. && \
    export ORT_CONFIG_DIR=.ort/config && \
    export ORT_DATA_DIR=.ort && \
	cd ort && \
	./gradlew cli:run --args="-c config.yml analyze -i $(ERL_TOP) -o . -f JSON --repository-configuration-file=.ort.yml" && \
	./gradlew cli:run --args="-c config.yml scan -o $(ERL_TOP) -f JSON -i ./cli/analyzer-result.json" && \
	./gradlew cli:run --args="report -i $(ERL_TOP)/scan-result.json -o $(ERL_TOP) -f SpdxDocument -O SpdxDocument=outputFileFormats=JSON"

# end
