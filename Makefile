# SPDX-FileCopyrightText: 2025 Erlang/OTP and its contributors
#
# SPDX-License-Identifier: Apache-2.0

##
# Project Title
#
# @file
# @version 0.1

.PHONY: test

all: sbom
clean:
	@-rm -rf otp ort otp/scan-result.json otp/bom.spdx.json bom.spdx.json

ort:
	git clone -b 49.0.0 https://github.com/oss-review-toolkit/ort.git ort

# e.g.,	REPO=https://github.com/erlang/otp.git, BRANCH=master
otp:
	git clone -b master https://github.com/erlang/otp.git otp

docker-build: otp ort
	docker build --tag sbom \
            --file "Dockerfile" \
            . \
			--network=host

# Run outside Docker
fix-sbom:
	./otp_compliance.escript sbom otp-info --sbom-file ort/cli/bom.spdx.json --input-file ort/cli/scan-result.json
	cp ort/cli/bom.spdx.json test/fixedSbom.spdx.json # Patched source SBOM
# make test

#
# grep -r -n1 "licenseConcluded\": \"NOASSERTION\"" bom.spdx.json | grep fileName \
# | cut -d\" -f4 | grep -v \.beam | grep -v \..md | grep -v ".*test.*/" | grep -v "/.*doc.*/" \
# | grep -v "/.*examples.*/" > files_without_beam_md_test_doc_examples.txt

#
# Run Docker commands
#
docker:
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

sbom:
	make job-install
	make job-gen-sbom
	make fix-sbom

test:
	diff test/bom.spdx.json test/fixedSbom.spdx.json

# Commands to run inside the docker container
analyze:
	cd ort && \
	./gradlew cli:run --args="-c /github/config.yml analyze -i /github/otp -o . -f JSON --repository-configuration-file=/github/.ort.yml"

scan:
	cd ort && \
	./gradlew cli:run --args="-c /github/config.yml scan -o . -f JSON -i /github/ort/cli/analyzer-result.json"

report:
	cd ort && \
	./gradlew cli:run --args="report -i /github/ort/cli/scan-result.json -o . -f SpdxDocument -O SpdxDocument=outputFileFormats=JSON" && \
	./gradlew cli:run --args="report -i /github/ort/cli/scan-result.json -o . --custom-license-texts-dir ../custom-license-texts --report-formats PlainTextTemplate"


# Job refers to a job in Jenkins
job-install:
	sudo apt-get update && sudo apt-get -y upgrade
	sudo -E apt-get install -y wget apt-transport-https gnupg build-essential git python3 python3-pip yarn nodejs python3-testresources
	export APT_KEY_DONT_WARN_ON_DANGEROUS_USAGE=1 && \
	wget -O - https://packages.adoptium.net/artifactory/api/gpg/key/public | sudo -E apt-key add -
	echo "deb https://packages.adoptium.net/artifactory/deb jammy main" | sudo -E tee /etc/apt/sources.list.d/adoptium.list
	sudo -E apt-get update
	sudo -E apt-get install -y temurin-21-jdk

	sudo -E apt-get install locales && sudo sed -i 's@# en_US.UTF-8@en_US.UTF-8@g' /etc/locale.gen && locale-gen && \
	sudo update-locale LANG=en_US.UTF-8

	pip install setuptools wheel
	pip install --proxy ${https_proxy} scancode-toolkit

job-gen-sbom: ort otp
	export CWD=`pwd` && \
	export ORT_CONFIG_DIR=$${CWD}/.ort/config && \
	export ORT_DATA_DIR=$${CWD}/.ort && \
	export PATH=${HOME}/.local/bin:${PATH} && \
	proxy=`echo "$${https_proxy}" | rev | cut -d: -f2-3 | rev | cut -d"/" -f3` && \
	port=`echo $${https_proxy} | cut -d: -f3` && \
	export JAVA_OPTS="$${JAVA_OPTS} -Dhttp.proxyHost=$${proxy} -Dhttp.proxyPort=$${port} -Dhttps.proxyHost=$${proxy} -Dhttps.proxyPort=$${port} -Xmx16g" && \
	cd ort && \
	./gradlew cli:run --args="-c $${CWD}/config.yml analyze -i $${CWD}/otp -o . -f JSON --repository-configuration-file=$${CWD}/.ort.yml" && \
	./gradlew cli:run --args="-c $${CWD}/config.yml scan -o . -f JSON -i $${CWD}/ort/cli/analyzer-result.json" && \
	./gradlew cli:run --args="report -i $${CWD}/ort/cli/scan-result.json -o . -f SpdxDocument,PlainTextTemplate -O SpdxDocument=outputFileFormats=JSON --custom-license-texts-dir $${CWD}/custom-license-texts"

# end
#

# ./gradlew cli:run --args="-c ../config.yml analyze -i ../mytest/master-opu -o . -f JSON --repository-configuration-file=../.ort.yml"


# TODO
# - Test apollo
#   ./gradlew cli:run --args="-c ../config.yml analyze -i ../mytest/master-opu -o . -f JSON --repository-configuration-file=../.ort.yml"

# - Add String parameter with BRANCH and REPO
