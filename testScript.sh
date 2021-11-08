#!/bin/bash
# ./rebuild.sh

rm -f ./outputs/*
stack run image-loop ./image/ ./outputs/
