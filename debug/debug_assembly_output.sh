#!/usr/bin/env bash

gcc \
  -m32 \
  -fomit-frame-pointer \
  -O3 debug_assembly_output.c \
  -S -o debug_assembly_output.s
