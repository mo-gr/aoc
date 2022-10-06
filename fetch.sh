#! /bin/zsh

YEAR=2017

curl -s https://adventofcode.com/$YEAR/day/$1/input -H "Cookie: session=`cat .session`" > data/$YEAR/AOC$1.input

git add data/$YEAR/AOC$1.input