#!/usr/bin/env bash

set -eu
set -o pipefail

dotnet run -v:m --project ./Pulumi.FSharp.Build/Pulumi.FSharp.Build.fsproj -- -t "$@"