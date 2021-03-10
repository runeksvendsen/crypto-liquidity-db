FROM ubuntu:16.04 as builder

RUN apt-get update \
  && apt-get install -y curl libpq-dev postgresql=9.5+173ubuntu0.3 postgresql-common libgmp10

# install stack only if necessary
RUN which stack || curl -sSL https://get.haskellstack.org/ | sh

# matches resolver in stack.yaml
# this step fails if performed after copying stack.yaml
RUN stack --resolver lts-14.17 setup

# copy dependency information
COPY cabal.project ./
COPY cabal.project.freeze ./
COPY stack.yaml ./
COPY stack.yaml.lock ./
COPY package.yaml ./package.yaml
# build library and executable dependencies
RUN stack build --dependencies-only

# copy test data
COPY test/data/double/test19.json ./test/data/double/test19.json
COPY test/data/regression/double-test19.txt ./test/data/regression/double-test19.txt
# build test dependencies
RUN stack build --dependencies-only --test --no-run-tests

COPY app ./app
COPY src ./src
COPY test ./test

# build+copy library, executables, tests
RUN stack build --test --no-run-tests --copy-bins --local-bin-path /tmp/dist/

RUN ls -l /tmp/dist/
