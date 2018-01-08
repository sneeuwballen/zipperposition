FROM ocaml/opam:alpine as build
# init and set perms
WORKDIR /zipper/build
RUN sudo chown opam: /zipper/build
# now install deps
RUN eval `opam config env`
RUN opam depext -i zarith
COPY --chown=opam:nogroup src *.opam Makefile ./
RUN opam pin add zipperposition.dev . -n
RUN opam install zipperposition --deps-only
RUN opam install jbuilder
# main build
RUN eval `opam config env` && make build
RUN cp _build/default/main/zipperposition.exe ./zipperposition

# prepare lightweight production image
FROM alpine:latest as prod
WORKDIR /root
RUN apk update # TODO remove this
RUN apk add gmp-dev
COPY --from=build /zipper/build/zipperposition .
ENTRYPOINT ["./zipperposition"]
