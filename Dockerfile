FROM sbtscala/scala-sbt:graalvm-ce-22.3.3-b1-java17_1.9.7_3.3.1 as builder

# Copy the build files
COPY . /build
WORKDIR /build

# Build the native image
RUN sbt cli/nativeImage

FROM scratch
COPY --from=builder /build/modules/cli/target/native-image/cli /cards
ENTRYPOINT [ "/cards" ]
