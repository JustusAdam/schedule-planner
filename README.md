# The schedule planner

Take an arbitrary list of weighted "lessons" and calculate the most ideal and valid layout for them based on weighing rules.

Base algorithm seems to work, will add UI soon.

## Install

- clone the repository `git clone https://github.com/JustusAdam/schedule-planner`
- install using cabal `cabal install`

## Usage

You may test it right now just executing the program `./dist/build/schedule-planner/schedule-planner filepath.json`

It currently works on raw rule and lesson data provided by the file `filepath.json`. You can find an example of such a file in `testsuite/test.json`.
