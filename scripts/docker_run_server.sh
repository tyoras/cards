#!/bin/bash

# run server against locally running database
docker run -it --rm --network=host --name cards_server tyoras/cards-server:latest
