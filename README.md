# Solutions to Advent of Code 2021

This repository contains my [Scala 3](https://www.scala-lang.org/) solutions to
the programming puzzels in [Advent of Code 2021](https://adventofcode.com/2021).

## How to run

To run one of the puzzles, just make sure Scala 3 is installed
[using SDKMAN](https://sdkman.io/sdks#scala) or
[other method](https://www.scala-lang.org/download/scala3.html), then run the
corresponding Scala file with example data file as argument. For example, the
puzzle-solution for [day 17](https://adventofcode.com/2021/day/17) is run like
this:

```console
$ scala 17/day17.scala 17/inputA.txt
Day 17 (Trick Shot) using file '17/inputA.txt'
target x            : Range 20 to 30
target y            : Range -10 to -5
candidate y init vel: Range 10 to -10 by -1
candidate x init vel: Range 6 to 30
max Y probe winner  : Probe(6,9)
max Y               : 45
distinct velocities : 112
```
