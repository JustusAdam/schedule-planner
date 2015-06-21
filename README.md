# The schedule planner [![Build Status](https://travis-ci.org/JustusAdam/schedule-planner.svg?branch=master)](https://travis-ci.org/JustusAdam/schedule-planner) [![Hackage version](https://img.shields.io/hackage/v/schedule-planner.svg)](https://hackage.haskell.org/package/schedule-planner)

Take an arbitrary list of weighted "lessons" and calculate the most ideal and valid layout for them based on weighing rules.

Base algorithm seems to work, web UI source code can be found [here](https://github.com/JustusAdam/schedule-planner-web) .

**There's a live site [here](http://justus.science/schedule-planner-web) that uses this software as backend, Try it out!**

## Install

- get the software
    - clone the repository `git clone https://github.com/JustusAdam/schedule-planner`  
    - or find the package on [Hackage](http://hackage.haskell.org/package/schedule-planner)
- install using cabal `cabal install`

## Usage

**The live [site](http://justus.science/schedule-planner-web) that uses this software as backend, Try it out!**

Execute it with `schedule-planner calc -i JSON_INPUT_FILE`.

By default it tries to obtain it's input data from `testsuite/test.json`, but you can specify any file as input using command line options.

Obtain information about the command line arguments using the `-h` or `--help` argument.

Since this is mostly intended to be used by myself to calculate a schedule and I attend the 'TU Dresden' there's a scraper script in `util/grab.py` that gets all the current lessons as json from the website.

## Code documentation

The Haddock documentation can be found on the [GitHub pages](http://justusadam.github.io/schedule-planner/)
