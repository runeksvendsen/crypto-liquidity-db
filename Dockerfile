## Dockerfile for "crypto-orderbook-db"
##
## This file builds an image containing all this project's executables.
##
## See Dockerfile's for individual executables under the subfolders
##  inside "exe/".

FROM ubuntu:16.04 as builder

RUN apt-get update \
  && apt-get install -y curl libpq-dev libgmp10

RUN curl -sSL https://get.haskellstack.org/ | sh

# Pre-install deps so we can re-use cached layers
# https://github.com/freebroccolo/docker-haskell/issues/54#issuecomment-283222910
COPY stack.yaml ./
COPY package.yaml ./
RUN stack setup
RUN stack install --dependencies-only

COPY src ./
COPY app ./

RUN stack build --test --copy-bins --local-bin-path /tmp/dist/

# RUNTIME
FROM ubuntu:16.04 as runtime

RUN apt-get update \
  && apt-get install -y ca-certificates libpq-dev libgmp10
COPY --from=builder /tmp/dist/* /usr/local/bin/
