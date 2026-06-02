# Card Games [![Pipeline](https://gitlab.com/tyoras/cards/badges/master/pipeline.svg)](https://gitlab.com/tyoras/cards/commits/master) [![Coverage](https://gitlab.com/tyoras/cards/badges/master/coverage.svg)](https://tyoras.gitlab.io/cards/coverage/) [![Docker](https://img.shields.io/badge/docker-image-blue.svg)](https://hub.docker.com/r/tyoras/cards)
Card game implementations using advanced [FP](https://en.wikipedia.org/wiki/Functional_programming) techniques in [Scala](https://www.scala-lang.org/).

The goal is to both have fun implementing these games and experimenting functional programming techniques.

## Architecture
The project is designed around a set of modules :
- `core` contains basic concepts useful for modeling a card game and games logic implementations.
- `persistence` contains the details related to the PostgreSQL persistence of the games.
- `cli` contains [CLI](https://en.wikipedia.org/wiki/Command-line_interface) oriented UIs for the games.
- `server` contains a web server exposing both restful APIs for managing the players and the games creation and websockets for playing the games.
- `shared` contains shared code between the server and clients.

## Finished games
- [War](docs/war.md)

## Work in progress
| Game                          | Logic | Local CLI | Web API | Remote CLI | persistence |
|-------------------------------|---|----------|---------|------------|------------|
| [Schnapsen](doc/schnapsen.md) | ✅ | ✅        | ❌ | ❌ | ❌ |
