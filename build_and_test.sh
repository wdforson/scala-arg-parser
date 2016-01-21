./build_jar.sh && \
mkdir -p test_classes && \
scalac -cp "ArgParser.jar" tests/*scala -d test_classes && \
scala -cp "ArgParser.jar:test_classes"  wdf.argParse.ArgParserTests && \
rm -rf test_classes
