##
# Project Title
#
# @file
# @version 0.1

install: ort otp docker-build

ort:
	git clone -b kiko/erlang-sbom https://github.com/kikofernandez/ort.git

otp:
	git clone -b maint https://github.com/erlang/otp.git

docker-build:
	docker build --tag sbom \
            --file "Dockerfile" \
            . \
			--network=host
	# docker run sbom "make "
	# docker pull eclipse-temurin:21-jdk-alpine
	# docker run -it -v $PWD/:/github --network host eclipse-temurin:21-jre-alpine /bin/sh

docker-analyze:
	docker run -v $PWD/:/github --network host sbom "cd /github && make analyze"

docker-scan:
	docker run -v $PWD/:/github --network host sbom "cd /github && make scan"

docker-report:
	docker run -v $PWD/:/github --network host sbom "cd /github && make report"


# TODO: add flags for config.yml and other files:
# - evaluator: license_classifications.yml
#
# Commands to run inside the docker container
analyze:
	cd ort && \
	./gradlew cli:run --args="-c /github/config.yml analyze -i /github/otp -o . -f JSON --repository-configuration-file=/github/.ort.yml"
    # ./gradlew cli:run --args="analyze -i $PWD/../otp -o . -f JSON --repository-configuration-file=$PWD/.ort.yml"

scan:
	cd ort && \
	./gradlew cli:run --args="-c /github/config.yml scan -o /github/otp -f JSON -i /github/ort/cli/analyzer-result.json"

report:
	cd ort && \
	./gradlew cli:run --args="report -i /github/otp/scan-result.json -o $PWD/../otp -f SpdxDocument -O SpdxDocument=outputFileFormats=JSON" && \
	./gradlew cli:run --args="report -i /github/otp/scan-result.json -o $PWD/../otp -f PlainTextTemplate -O PlainTextTemplate=template.path=$PWD/../otp-sbom/COPYRIGHT.md.ftl"

fix-sbom:
	./otp_compliance.escript sbom otp-info --sbom-file bom.spdx.json --input-file scan-result.json


all: install
# end
