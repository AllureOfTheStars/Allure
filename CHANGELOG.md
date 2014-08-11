## [v0.4.9.0, aka 'Player escapes through airlock'](https://github.com/LambdaHack/LambdaHack/compare/v0.4.14...v0.4.9.0)

- balance game content a bit (campaign still unbalanced)
- fix a bug where doors can't be closed
- assign AI tactics to players, in particular use follow-the-leader in safari
- specify monster spawn rate per-cave
- generally update content to the new v0.5.0.0 of LambdaHack content API

## [v0.4.14, aka 'Out of cosmic balance'](https://github.com/AllureOfTheStars/Allure/compare/v0.4.12...v0.4.14)

- add tons of new (unbalanced) items, actors and descriptions
- add a simple cabal test in addition to make-test and travis-test
- add items of Wonder and of Marvel
- add game mechanics, items and places to enable stealthy tactics
- add lots of shrapnel (explosions) and organs (body parts)
- expose a bit of the plot via new game modes and their order

## [v0.4.12](https://github.com/AllureOfTheStars/Allure/compare/v0.4.10...v0.4.12)

- make walls lit by default to simplify exploration
- improve and simplify dungeon generation
- simplify running and permit multi-actor runs
- let items explode and generate shrapnel projectiles
- add game difficulty setting (initial HP scaling right now)
- allow recording, playing back and looping commands
- implement pathfinding via per-actor BFS over the whole level
- extend setting targets for actors in UI tremendously
- implement autoexplore, go-to-target, etc., as macros
- let AI use pathfinding, switch leaders, pick levels to swarm to
- force level/leader changes on spawners (even when played by humans)
- extend and redesign UI bottom status lines

## [v0.4.10](https://github.com/AllureOfTheStars/Allure/compare/v0.4.8...v0.4.10)

- screensaver game modes (AI vs AI)
- improved AI (can now climbs stairs, etc.)
- multiple, multi-floor staircases
- multiple savefiles
- configurable framerate and combat animations

## [v0.4.8](https://github.com/AllureOfTheStars/Allure/compare/v0.4.6.5...v0.4.8)

- experimental multiplayer modes
- a lot of gameplay changes induced by the engine overhaul and in particular the client-server rewrite

## [v0.4.6.5](https://github.com/AllureOfTheStars/Allure/compare/v0.4.6...v0.4.6.5)

- this is a minor release, primarily intended to fix the broken compilation on Hackage
- changes since 0.4.6 are mostly unrelated to gameplay:
    - strictly typed config files split into UI and rules
    - a switch from Text to String throughout the codebase
    - use of the external library miniutter for English sentence generation

## [v0.4.6](https://github.com/AllureOfTheStars/Allure/compare/v0.4.4...v0.4.6)

- the Main Menu
- improved and configurable mode of squad combat

## [v0.4.4](https://github.com/AllureOfTheStars/Allure/compare/v0.4.3...v0.4.4)

- missiles flying for three turns (by an old kosmikus' idea)
- visual feedback for targeting
- animations of combat and individual monster moves

## [v0.4.3](https://github.com/AllureOfTheStars/Allure/compare/v0.4.2...v0.4.3)

- the Allure of the Stars game depends on the LambdaHack engine library
