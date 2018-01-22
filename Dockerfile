FROM ocaml/opam:alpine as build
# init and set perms
WORKDIR /zipper/build
RUN sudo chown opam: /zipper/build
# deps
RUN eval `opam config env` && \
    opam update && \
    opam depext -i zarith && \
    opam install jbuilder zarith containers sequence msat menhir
# main build
COPY --chown=opam:nogroup src *.opam Makefile ./
RUN eval `opam config env` && \
    make build && \
    cp _build/default/main/zipperposition.exe ./zipperposition

# prepare lightweight production image
FROM alpine:latest as prod
WORKDIR /root
RUN apk update && apk add gmp-dev
COPY --from=build /zipper/build/zipperposition .
ENTRYPOINT ["./zipperposition"]
