#!/bin/bash
while true; do sed -i "s/let _ = [0-9]*/let _ = ${RANDOM}/" Library.fs; dotnet build -v q; done
