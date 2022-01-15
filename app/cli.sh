#!/bin/bash

elm make src/Main.elm --output=src/main.js
node src/repl.js