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
COPY crypto-liquidity-db.cabal ./crypto-liquidity-db.cabal
COPY LICENSE ./LICENSE
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

### Build progressively more stuff to improve usefulness of Docker layer caching
# Build library deps
RUN stack build --only-dependencies crypto-liquidity-db:lib
# Build library
RUN stack build crypto-liquidity-db:lib
# Build test suite deps
RUN stack build --only-dependencies --test --no-run-tests crypto-liquidity-db:test:crypto-liquidity-db-test
# Build test suite
RUN stack build --test --no-run-tests crypto-liquidity-db:test:crypto-liquidity-db-test
# Copy test suite binary
RUN mkdir -p /tmp/dist/ && cp "$(find . -name crypto-liquidity-db-test -type f)" /tmp/dist/
# Build executable deps
RUN stack build --only-dependencies
# Build executables and copy binaries
RUN stack build --copy-bins --local-bin-path /tmp/dist/

# RUNTIME
FROM ubuntu:16.04 as runner

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    ca-certificates libpq-dev postgresql=9.5+173ubuntu0.3 postgresql-common libgmp10

# clean up
RUN apt-get clean autoclean && apt-get autoremove --yes && rm -rf /var/lib/{apt,dpkg,cache,log}/ /usr/share/{doc,man,locale}

# copy all executables (includes test executable)
COPY --from=builder /tmp/dist/* /usr/local/bin/

COPY test/data/ ./test/data/
COPY pgsql ./pgsql