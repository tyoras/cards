FROM sbtscala/scala-sbt:graalvm-community-25.0.1_1.12.0_3.7.4 AS builder

# Copy the build files
COPY . /build
WORKDIR /build

# Build the native image
RUN sbt cli/nativeImage

FROM scratch
COPY --from=builder /build/modules/cli/target/native-image/cli /cards
ENTRYPOINT [ "/cards" ]
