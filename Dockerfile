FROM ubuntu:16.04 as builder

RUN apt-get update \
  && apt-get install -y curl libpq-dev postgresql=9.5+173ubuntu0.3 postgresql-common libgmp10

# install stack only if necessary
RUN which stack || curl -sSL https://get.haskellstack.org/ | sh

# matches resolver in stack.yaml
# this step fails if performed after copying stack.yaml
RUN stack --resolver lts-14.17 setup

# required by dependency: gargoyle-postgresql-nix
ENV PATH="/usr/lib/postgresql/9.5/bin:$PATH"

COPY cabal.project ./
COPY cabal.project.freeze ./
COPY stack.yaml ./
COPY stack.yaml.lock ./

# copy dependency information
COPY package.yaml ./package.yaml

RUN stack install --dependencies-only

COPY test/data/double/test19.json ./test/data/double/test19.json
COPY test/data/regression/double-test19.txt ./test/data/regression/double-test19.txt
RUN stack build --dependencies-only --test --no-run-tests

COPY app ./app
COPY src ./src
COPY test ./test

RUN stack build --test --no-run-tests --copy-bins --local-bin-path /tmp/dist/

# RUNTIME
FROM ubuntu:16.04 as runner

RUN apt-get update \
  && apt-get install -y ca-certificates libpq-dev postgresql=9.5+173ubuntu0.3 postgresql-common libgmp10

# clean up
RUN apt-get clean autoclean
RUN apt-get autoremove --yes
RUN rm -rf /var/lib/{apt,dpkg,cache,log}/

# copy all executables
COPY --from=builder .stack-work/dist/x86_64-linux/Cabal-2.4.0.1/build/crypto-liquidity-db-test/crypto-liquidity-db-test /usr/local/bin/
COPY --from=builder /tmp/dist/crypto-liquidity-web-api /usr/local/bin/
COPY --from=builder /tmp/dist/crypto-liquidity-service-process /usr/local/bin/
COPY --from=builder /tmp/dist/crypto-liquidity-service-create /usr/local/bin/

COPY test/data/ ./test/data/
COPY pgsql ./pgsql