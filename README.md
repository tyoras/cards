# Card Games [![Pipeline](https://gitlab.com/tyoras/cards/badges/master/pipeline.svg)](https://gitlab.com/tyoras/cards/commits/master) [![Coverage](https://gitlab.com/tyoras/cards/badges/master/coverage.svg)](https://tyoras.gitlab.io/cards/coverage/) [![Docker](https://img.shields.io/badge/docker-image-blue.svg)](https://hub.docker.com/r/tyoras/cards)
Card game implementations using advanced [FP](https://en.wikipedia.org/wiki/Functional_programming) techniques in [Scala](https://www.scala-lang.org/).

The goal is to both have fun implementing these games and experimenting functional programming techniques.  

## Architecture
The project is designed around a set of modules :
- `core` contains basic concepts useful modeling a card game and games logic implementations.
- `config` contains the game logic implementations.
- `persistence` contains the details related to the postgresql persistence of the games.
- `cli` contains a [cli](https://en.wikipedia.org/wiki/Command-line_interface) oriented UI for the games.
- `server` (Work in progress) contains a web server for playing the games via restful apis.

## Finished games
None yet.

## Work in progress
### Schnapsen
[Schnapsen](https://en.wikipedia.org/wiki/Schnapsen) is an austrian game, it is the first game I have decided to implement because of its medium complexity which is a good first challenge.

Its game logic implementation is designed as a [FSM](https://en.wikipedia.org/wiki/Finite-state_machine).
#### Game rules references
- [Schnapsen Rules (Wikipedia)](https://en.wikipedia.org/wiki/Schnapsen)
- [Schnapsen Rules](https://www.pagat.com/marriage/schnaps.html)
 
### War
The classic and basic game.
#### Game rules references
- [War Rules (Wikipedia)](https://en.wikipedia.org/wiki/War_(card_game))
