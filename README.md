# Jellyfish Solution

See original requirements [here](JELLYFISH-README.md).

## Assumptions

The only major assumption relates to parsing coordinates, specifically to satisfy the following two requirements:

> A Jellyfish position consists of a grid coordinate (a pair of integers: x-coordinate followed by y-coordinate)

> The maximum value for any coordinate is 60

Coordinates are therefore assumed to be in any one of the following formats, up to a maximum of `6060`:

* xy
* xxy
* xxyy

## Prerequisites

The dotnet core 3.1 SDK needs to be installed on the host system in order to build.

## Build

The solution uses [FAKE 5](https://fake.build/) for building and running unit tests. 

Run the following to build the solution and execute unit tests:

```
dotnet tool restore
dotnet fake build
```

Building and running unit tests can also be done through Visual Studio.

## Run

Assemblies/executables are built to the `build_output` folder.

For convenience, a packaged windows executable is included in `packaged`.

### Command Line Arguments

By default, the application runs interactively by prompting for input. Alternatively, an instructions file can be specified. See below for detailed usage:

```
USAGE: Jellyfish.Console.exe [--help] [--file <string>] [--history]

OPTIONS:

    --file, -f <string>   Specify an instructions file
    --history, -h         Output a history of jellyfish positions
    --help                display this list of options.
```