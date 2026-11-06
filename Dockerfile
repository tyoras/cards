FROM sbtscala/scala-sbt:graalvm-community-25.0.2_1.13.0_3.9.0 AS builder

ARG MODULE

# Using recommended musl toolchain in order to allow building a statically linked native image
RUN mkdir /musl && \
  curl -SLO https://gds.oracle.com/download/bfs/archive/musl-toolchain-1.2.5-oracle-00001-linux-amd64.tar.gz && \
  tar -C /musl -xzf musl-toolchain-1.2.5-oracle-00001-linux-amd64.tar.gz

ENV PATH="/musl/musl-toolchain/bin:$PATH"
# Use the GraalVM shipped in the image instead of letting sbt-native-image download another one
ENV NATIVE_IMAGE_INSTALLED=true

# Copy the build files
COPY . /build
WORKDIR /build

# Build the native image
RUN sbt $MODULE/nativeImage

# Used by the CI: packages a native image already built by the `build_native` job (skips the builder stage)
FROM alpine AS prebuilt

ARG MODULE
COPY modules/$MODULE/target/native-image/$MODULE /cards
ENTRYPOINT [ "/cards" ]

# Default target: builds the native image from sources
FROM alpine

ARG MODULE
COPY --from=builder /build/modules/$MODULE/target/native-image/$MODULE /cards
ENTRYPOINT [ "/cards" ]
