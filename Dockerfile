## Dockerfile for "crypto-orderbook-db"
##
## This file builds an image containing all this project's executables.
##
## See Dockerfile's for individual executables under the subfolders
##  inside "exe/".

FROM build-cache-crypto-liquidity:0.1 as builder

RUN apt-get update \
  && apt-get install -y curl libpq-dev postgresql=9.5+173ubuntu0.3 postgresql-common libgmp10

# Pre-install deps so we can re-use cached layers
# https://github.com/freebroccolo/docker-haskell/issues/54#issuecomment-283222910
COPY stack.yaml ./
COPY package.yaml ./
RUN stack setup

# required by dependency: gargoyle-postgresql-nix
ENV PATH="/usr/lib/postgresql/9.5/bin:$PATH"

RUN stack install --dependencies-only

COPY src ./src
COPY test ./test
COPY app ./app

RUN stack build --test --copy-bins --local-bin-path /tmp/dist/

# RUNTIME
FROM ubuntu:16.04 as runtime

RUN apt-get update \
  && apt-get install -y ca-certificates libpq-dev postgresql=9.5+173ubuntu0.3 postgresql-common libgmp10
COPY --from=builder /tmp/dist/* /usr/local/bin/

COPY pgsql ./pgsql

ENTRYPOINT [ "crypto-liquidity-db-exe" ]
