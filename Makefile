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

# Run outside Docker
fix-sbom:
	./otp_compliance.escript sbom otp-info --sbom-file /github/otp/bom.spdx.json --input-file /github/otp/scan-result.json
	cp /github/otp/bom.spdx.json . # Patched source SBOM

#
# Run Docker commands
#
docker-all: docker-analyze docker-scan docker-report

docker-analyze:
	docker run -v $PWD/:/github --network host sbom "cd /github && make analyze"

docker-scan:
	docker run -v $PWD/:/github --network host sbom "cd /github && make scan"

docker-report:
	docker run -v $PWD/:/github --network host sbom "cd /github && make report"


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


all: install docker-all fix-sbom
# end
