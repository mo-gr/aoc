#! /bin/zsh

YEAR=2021

curl -s https://adventofcode.com/$YEAR/day/$1/input -H "Cookie: session=`cat .session`" > data/$YEAR/AOC$1.input
