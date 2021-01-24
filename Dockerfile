FROM ghcr.io/graalvm/graalvm-ce:ol7-java11-21.0.0 as builder
RUN gu install native-image
RUN curl https://bintray.com/sbt/rpm/rpm | tee /etc/yum.repos.d/bintray-sbt-rpm.repo && \
    yum install -y sbt
COPY . /build
WORKDIR /build
RUN curl -L -o musl.tar.gz \
    https://github.com/gradinac/musl-bundle-example/releases/download/v1.0/musl.tar.gz && \
    tar -xvzf musl.tar.gz
RUN sbt clean compile
RUN sbt cli/graalvm-native-image:packageBin
RUN curl -L -o docker-latest.tgz http://get.docker.com/builds/Linux/x86_64/docker-latest.tgz
RUN tar -xvzf docker-latest.tgz

FROM scratch
COPY --from=builder /build/cli/target/graalvm-native-image/cli /cards
COPY --from=builder /build/docker/docker /docker
ENTRYPOINT [ "/cards" ]