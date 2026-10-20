#!/bin/bash

echo "library(weaver); Sweave(\"$1\", driver=weaver())" | R --no-save --no-restore
