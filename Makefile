##
# Project Title
#
# @file
# @version 0.1

cwd=$(PWD)

install: ort otp

ort:
	git clone -b kiko/erlang-sbom https://github.com/kikofernandez/ort.git

otp:
	git clone -b maint https://github.com/erlang/otp.git

# TODO: add flags for config.yml and other files:
# - evaluator: license_classifications.yml
analyze:
	cd ort && \
	./gradlew cli:run --args="-c $(cwd)/config.yml analyze -i $(cwd)/otp -o . -f JSON --repository-configuration-file=$(cwd)/.ort.yml"
    # ./gradlew cli:run --args="analyze -i $PWD/../otp -o . -f JSON --repository-configuration-file=$PWD/.ort.yml"

scan:
	./../ort/gradlew cli:run --args="scan -o $PWD/../otp -f JSON -i $PWD/cli/analyzer-result.json"

report:
	../ort/gradlew cli:run --args="report -i $PWD/../otp/scan-result.json -o $PWD/../otp -f SpdxDocument -O SpdxDocument=outputFileFormats=JSON"
	../ort/gradlew cli:run --args="report -i $PWD/../otp/scan-result.json -o $PWD/../otp -f PlainTextTemplate -O PlainTextTemplate=template.path=$PWD/../otp-sbom/COPYRIGHT.md.ftl"

fix-sbom:
	./otp_compliance.escript sbom otp-info --sbom-file bom.spdx.json --input-file scan-result.json

docker-build:
	docker build --tag sbom \
            --build-arg MAKEFLAGS=-j$(($(nproc) + 2)) \
            --file "Dockerfile" \
            .
	# docker run sbom "make "
	# docker pull eclipse-temurin:21-jdk-alpine
	# docker run -it -v $PWD/:/github --network host eclipse-temurin:21-jre-alpine /bin/sh

all: install
# end
