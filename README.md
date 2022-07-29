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

### GraalVM native image
Additionally building a native binary using GraalVM is possible:
    
    $ export GRAALVM_HOME=/usr/lib/jvm/java-17-graalvm/bin # this path depends on your GraalVM installation
    $ $GRAALVM_HOME/gu install native-image # install the native image component
    $ lein native-image

## Examples
TODO

## Bugs
Not all features from ISO 8601-2:2019 are supported, missing are e.g.:
- grouped units
- selection rules

Expressions are currently not validated, it is therefore easy to produce accepted but invalid expressions.

## Further information
https://en.wikipedia.org/wiki/ISO_8601

freely available related standards:
- https://www.loc.gov/standards/datetime/
- https://standards.calconnect.org/csd/cc-18011.html

## License
AGPLv3
