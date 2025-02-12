#!/usr/bin/env bash
trap 'kill $(jobs -p)' EXIT
# spago bundle-app --watch --to dev/index.js 2>&1 | cat &
# esbuild dev/index.js --servedir=dev --watch=forever --outfile=dev/index.js 2>&1 | cat &
npm run watch 2>&1 | cat &
npm run serve 2>&1 | cat &
wait
