# explain8601
Convert an ISO 8601 expression to a human readable description. ISO 8601-1:2019 and parts of ISO 8601-2:2019 are supported, older versions of the standard and RFC 3339 are not supported.

## Installation
You can download a release as a jar file from the GitHub releases page. Alternatively see the build section below.

## Usage
The basic usage is

    $ java -jar explain8601-0.1.0-standalone.jar [opts] -e expression

Use the ``-d`` option to show the intermediate results of the conversion.

## Build
Clojure and Leiningen are required to build a jar file, after cloning this repo run:

    $ lein uberjar

## Test
Run all tests with

    $ lein test

### GraalVM native image
Additionally building a native binary using GraalVM is possible:
    
    $ export GRAALVM_HOME=/usr/lib/jvm/java-17-graalvm/bin # this path depends on your GraalVM installation
    $ $GRAALVM_HOME/gu install native-image # install the native image component
    $ lein native-image

## Examples
The following is a list of generated descriptions:
- '2022-22' represents the summer of 2022.
- '2022/2023' represents an interval from the year 2022 to the year 2023.
- 'P1YP2W' represents a duration of 1 year and 2 weeks.
- 'R2/2022-123T00:00/PT1H' represents an interval from the 123rd day of the year 2022 at 0 minutes past 0 hours, lasting for a duration of 1 hour, repeating 2 times.
- '2022Y7M30~DT1S' represents the 30th day (approximate) of july of the year 2022 at 1 second past 0 hours.

## Bugs
Not all features from ISO 8601-2:2019 are supported, missing are e.g.:
- grouped units
- selection rules
- ranges (``..``)

Expressions are currently not completely validated, it is therefore easy to produce accepted but invalid expressions.

## Further information
https://en.wikipedia.org/wiki/ISO_8601

freely available related standards:
- https://www.loc.gov/standards/datetime/
- https://standards.calconnect.org/csd/cc-18011.html

## License
AGPLv3
