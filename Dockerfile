FROM sbtscala/scala-sbt:graalvm-ce-22.3.0-b2-java17_1.8.2_3.2.2 as builder

# Copy the build files
COPY . /build
WORKDIR /build

# Build the native image
RUN sbt cli/nativeImage

FROM scratch
COPY --from=builder /build/modules/cli/target/native-image/cli /cards
ENTRYPOINT [ "/cards" ]
