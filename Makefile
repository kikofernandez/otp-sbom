##
# Project Title
#
# @file
# @version 0.1

download_ort:
	git clone -b kiko/add-license-info-from-files/GH-8485 git@github.com:kikofernandez/ort.git

install-prerequisites: adoptium

otp:
	git clone git@github.com:erlang/otp.git

adoptium:
	sudo apt-get install -y temurin-21-jdk

ort:
	git clone git@github.com:kikofernandez/ort.git && git checkout kiko/erlang-sbom

# TODO: add flags for config.yml and other files:
# - evaluator: license_classifications.yml
analyze:
	./../ort/gradlew cli:run --args="analyze -i $PWD/otp -o . -f JSON --repository-configuration-file=$PWD/.ort.yml"
    # ./gradlew cli:run --args="analyze -i $PWD/../otp -o . -f JSON --repository-configuration-file=$PWD/.ort.yml"

scan:
	./../ort/gradlew cli:run --args="scan -o $PWD/../otp -f JSON -i $PWD/cli/analyzer-result.json"

report:
	../ort/gradlew cli:run --args="report -i $PWD/../otp/scan-result.json -o $PWD/../otp -f SpdxDocument -O SpdxDocument=outputFileFormats=JSON"
	../ort/gradlew cli:run --args="report -i $PWD/../otp/scan-result.json -o $PWD/../otp -f PlainTextTemplate -O PlainTextTemplate=template.path=$PWD/../otp-sbom/COPYRIGHT.md.ftl"

fix-sbom:
	./otp_compliance.escript sbom otp-info --sbom-file bom.spdx.json --input-file scan-result.json

all: adoptium otp analyze scan report fix-sbom
# end
