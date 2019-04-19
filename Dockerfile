FROM ocaml/opam2:alpine-3.8-ocaml-4.06 as build
# init and set perms
WORKDIR /zipper/build
RUN sudo apk update
RUN sudo chown opam: /zipper/build
# deps
RUN eval `opam config env` && \
    cd /home/opam/opam-repository && \
    git pull && \
    opam update && \
    opam depext -i zarith && \
    opam install dune zarith containers iter msat menhir
# main build
COPY --chown=opam:nogroup src *.opam Makefile dune-project ./
RUN eval `opam config env` && \
    make build && \
    cp _build/default/main/zipperposition.exe ./zipperposition

# prepare lightweight production image
FROM alpine:latest as prod
WORKDIR /root
RUN apk update && apk add gmp-dev
COPY --from=build /zipper/build/zipperposition .
ENTRYPOINT ["./zipperposition"]
