#!/bin/sh
curl -i -X POST -d "@$1" --header "Content-Type: application/json" localhost:8081/user
