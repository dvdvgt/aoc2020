#!/bin/bash

cd "$(dirname "$0")"

if [ -r .cookie ]; then
  . .cookie
fi

export TZ=EST
thisyear="$(date +%Y)"
thismonth="$(date +%m)"
thisday="$(date +%d)"

echo $1

year=2020
if [ "$1" = "" ]; then
  for day in {1..25}; do
    if [ "$thisyear" -ne "$year" -o "$thismonth" -ne 12 -o "$day" -gt "$thisday" ]; then
      exit 0
    fi
    filename="input$day".txt
    if [ -r "$filename" ]; then
      continue  # make sure we don't fetch the same file twice!
    fi
    curl -sS -o "input/$filename" -b "$AOC_COOKIE" https://adventofcode.com/"$year"/day/"$day"/input
  done
else
  if [ $1 -gt 0 ] && [ $1 -lt 25 ]; then
    filename="input$1".txt
    if [ ! -r "$filename" ]; then
      curl -sS -o "input/$filename" -b "$AOC_COOKIE" https://adventofcode.com/"$year"/day/"$1"/input
    fi
  fi
fi