FROM sbtscala/scala-sbt:graalvm-ce-22.3.0-b2-java17_1.8.2_3.2.2 as builder

# Install native-image
RUN gu install native-image

# Static image requirements
RUN mkdir /staticlibs && \
    curl -L -o musl.tar.gz https://musl.libc.org/releases/musl-1.2.3.tar.gz && \
    mkdir musl && tar -xvzf musl.tar.gz -C musl --strip-components 1 && cd musl && \
    ./configure --disable-shared --prefix=/staticlibs && \
    make && make install && \
    cd / && rm -rf /muscl && rm -f /musl.tar.gz

ENV PATH="$PATH:/staticlibs/bin"
ENV CC="musl-gcc"

RUN curl -L -o zlib.tar.gz https://zlib.net/zlib-1.2.13.tar.gz && \
    mkdir zlib && tar -xvzf zlib.tar.gz -C zlib --strip-components 1 && cd zlib && \
    ./configure --static --prefix=/staticlibs && \
    make && make install && \
    cd / && rm -rf /zlib && rm -f /zlib.tar.gz

# Copy the build files
COPY . /build
WORKDIR /build

# Build the native image
RUN sbt clean compile
RUN sbt cli/graalvm-native-image:packageBin

FROM scratch
COPY --from=builder /build/modules/cli/target/graalvm-native-image/cli /cards
ENTRYPOINT [ "/cards" ]
