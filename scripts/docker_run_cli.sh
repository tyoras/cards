#!/bin/bash

# Run cli against locally running cards server
docker run -it -e CARDS_CLI_PASSWORD=fake --rm  --network=host --name cards_cli tyoras/cards:latest war
