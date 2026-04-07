#!/bin/bash

docker build -t tyoras/cards-server:latest --build-arg MODULE=server --pull .
