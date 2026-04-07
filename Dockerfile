FROM sbtscala/scala-sbt:graalvm-community-25.0.1_1.12.8_3.8.3 AS builder

ARG MODULE

# Using recommended musl toolchain in order to allow building a statically linked native image
RUN mkdir /musl && \
  curl -SLO https://gds.oracle.com/download/bfs/archive/musl-toolchain-1.2.5-oracle-00001-linux-amd64.tar.gz && \
  tar -C /musl -xzf musl-toolchain-1.2.5-oracle-00001-linux-amd64.tar.gz

ENV PATH="/musl/musl-toolchain/bin:$PATH"

# Copy the build files
COPY . /build
WORKDIR /build

# Build the native image
RUN sbt $MODULE/nativeImage

FROM alpine

ARG MODULE
COPY --from=builder /build/modules/$MODULE/target/native-image/$MODULE /cards
ENTRYPOINT [ "/cards" ]
