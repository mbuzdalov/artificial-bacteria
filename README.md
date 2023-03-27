Artificial Bacteria

## Description

The code is a sort of a system where bacteria live (they can fork, eat, move
and die). Every bacteria has a genome, which is the program it uses to live.
The length of the genome has an impact on its life, since it becomes harder
to move and more energy is burned even when not moving.

## Installing and running

The project is written in Scala, the current version is `2.13.10` (any `2.13.x` will do).
Scala itself runs on a Java virtual machine. Java versions `1.8` and `11` have been tested.
For the user interface, the project uses Swing, and for sound the `javax.sound.sampled`
infrastructure is used. Other than the standard libraries, the project has no dependencies.

The project uses `sbt`. To start, type `sbt run` in the console.
A rather elaborate configuration is availave in `config.properties`
with most parameters reasonably well commented.

## History

The project started in 2016 during the GECCO conference, inspired by some talk
(which one, I cannot currently remember). In a sense, I see it as a part of
a large meta-project about the simplest possible rules that enable the proper
evolution: of course this project is far from having simplest rules, but some
features, such as the minimim field size requirements, can be seen even at this level.

In 2017-2018, it was reinvented as a separate codebase from the big
framework for evolutionary algorithms I was then developing. Some time has been
spent on improving the environment and making it more dynamic, such that the evolution
has something to chew on. This is probably the time when some interesting behaviors
started to evolve.

In 2020-2021, this project evolved to have a more user-friendly GUI,
which made it possible to present it at few Russian art exhibitions as an installation.
During that time, more interaction with the field was added, such as the
possibility to add some food by clicking on the field, to "nuke" everything
in a radius, to restart the whole thing, and even to have a sound which is
directly derived from the positions and health status of the bacteria.

In 2022, this was put to GitHub and converted to an sbt-based project,
so that a larger audience can experiment with it. In 2023, the user interface
was translated to English, so that a larger audience can actually understand
what happens :)

## Notes

In the current configuration, the system is initially filled with rather dumb
species, some of them eventually learn to move towards more food, but at some
point of time a good combination of qualities finally evolves in one of bacteria,
and its clones conquer the local universe and eat a large chunk of food.
The genome becomes quite large at this point. After that, the bacteria are
gradually optimized to take less genome space and to behave better, so really
good genomes evolve after e.g. a day of evolution.
