FROM alpine:3.18.3

RUN apk add --no-cache \
    bash \
    coreutils \
    curl \
    git \
    gzip \
    make \
    npm \
    openjdk11-jre \
    tmux \
    vim

RUN curl -L https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz \
    | gunzip >/usr/local/bin/elm \
    && chmod +x /usr/local/bin/elm

WORKDIR /install
RUN npm install \
    elm-format \
    elm-live \
    firebase-tools
ENV PATH=$PATH:/install/node_modules/.bin
RUN echo 'PATH=$PATH:/install/node_modules/.bin' > /etc/profile

WORKDIR /workdir/functions
COPY functions/package.json functions/package-lock.json /workdir/functions/
RUN npm ci
VOLUME /workdir/functions/node_modules


ENV HOME=/home
WORKDIR /workdir

RUN touch /.inside_docker_container
