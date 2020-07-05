#!/bin/bash

#trap ctrl_c INT

#PIPE=$(mktemp -u)
#mkfifo $PIPE

function ctrl_c {
    echo -n stop > $PIPE
}

#while [[ $(read -st 0.01 line <> "$PIPE") != "stop" ]]; do sed -i "s/let _ = [0-9]*/let _ = ${RANDOM}/" Library.fs; dotnet build --nologo -v q; done
while true; do sed -i "s/let _ = [0-9]*/let _ = ${RANDOM}/" Library.fs; dotnet build --nologo -v q; done
