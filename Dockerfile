FROM ocaml/opam:alpine-3.15-ocaml-4.12-flambda as build
# init and set perms
WORKDIR /zipper/build
RUN sudo apk update
RUN sudo chown opam: /zipper/build
COPY --chown=opam:nogroup *.opam Makefile dune-project ./
# deps
RUN eval `opam env` && \
    opam depext -i zarith && \
    opam install logtk libzipperposition zipperposition --deps-only
# main build
COPY --chown=opam:nogroup src ./src
RUN eval `opam env` && \
    make build && \
    cp _build/default/src/main/zipperposition.exe ./zipperposition

# prepare lightweight production image
FROM alpine:latest as prod
WORKDIR /root
RUN apk update && apk add gmp-dev
COPY --from=build /zipper/build/zipperposition .
ENTRYPOINT ["./zipperposition"]
