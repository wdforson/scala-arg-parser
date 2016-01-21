mkdir -p classes && \
scalac ArgParser.scala ArgSpec.scala -d classes && \
jar cf ArgParser.jar -C classes . && \
rm -rf classes
