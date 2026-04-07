#!/bin/bash

docker build -t tyoras/cards:latest --build-arg MODULE=cli --pull .
