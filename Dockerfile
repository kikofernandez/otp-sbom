# FROM eclipse-temurin:21-jdk-alpine
FROM ubuntu:22.04
# docker run -it -v $PWD/:/github --network host --build-arg UID=$(id -u) --build-arg GID=$(id -g) eclipse-temurin:21-jdk-alpine /bin/sh

USER root

# Install build tools
RUN apt-get update && apt-get -y upgrade

# Install Temurin
RUN apt-get install -y wget apt-transport-https gnupg && \
    wget -O - https://packages.adoptium.net/artifactory/api/gpg/key/public | apt-key add - && \
    echo "deb https://packages.adoptium.net/artifactory/deb $(awk -F= '/^VERSION_CODENAME/{print$2}' /etc/os-release) main" | tee /etc/apt/sources.list.d/adoptium.list && \
    apt-get update && \
    apt-get install -y temurin-21-jdk

# Install Other Essentials
RUN apt-get install -y build-essential git python3 python3-pip python3.10-venv yarn nodejs
# nodejs is needed for web app template plugin in ORT.

# Install scancode
RUN pip install --upgrade pip setuptools wheel && pip install scancode-toolkit

RUN apt-get install locales && sed -i 's@# en_US.UTF-8@en_US.UTF-8@g' /etc/locale.gen && locale-gen

RUN update-locale LANG=en_US.UTF-8
ENV LC_ALL en_US.UTF-8
