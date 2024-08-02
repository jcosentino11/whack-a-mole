FROM erlang:alpine AS build

WORKDIR /work

RUN apk add --no-cache \
    build-base

COPY Makefile rebar.config rebar.lock ./
COPY src ./src
COPY priv ./priv

RUN make ENV=prod release

FROM alpine AS release

RUN apk add --no-cache \
    openssl \
    ncurses-libs \
    libstdc++

COPY --from=build /work/_build/prod/rel/whackamole /whackamole

EXPOSE 8080

CMD ["/whackamole/bin/whackamole", "foreground"]
