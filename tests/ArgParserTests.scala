package wdf.argParse

object ArgParserTests {
  val argA = new StringArg(Set("--a", "-a"), required=true)
  val argB = new StringArg(Set("--bArg", "-b"))
  val argC = new IntArg(Set("--cArg", "-c"), required=true)
  val argD = new BoolArg(Set("--dArg", "-d"))
  val argMulti = new MultiStringArg(Set("--multiArg", "-m"))

  // scenario 1: all args present explicitly, along with some interspersed extras
  val args1 = Array("pos1", "--cArg", "42", "pos2", "pos3", "-a", "aValue", "-d", "--multiArg", "m1", "m2", "m3", "-b", "bVal", "pos4")
  val expected1 = Map[ArgSpec[_], Any](argA -> "aValue", argB -> "bVal", argC -> 42, argD -> true, argMulti -> List("m1", "m2", "m3"))
  val expectedPos1 = List("pos1", "pos2", "pos3", "pos4") 

  def testScenario1(parser: ArgParser) {
    val parsed = parser.parseArgs(args1)
    checkArgs(expected1, expectedPos1, parsed)
  }

  // scenario 1a: all args present explicitly, along with some interspersed extras,
  //              and 'true' is passed for '-d'
  val args1a = Array("pos1", "--cArg", "42", "pos2", "pos3", "-a", "aValue", "-d", "true", "--multiArg", "m1", "m2", "m3", "-b", "bVal", "pos4")

  def testScenario1a(parser: ArgParser) {
    val parsed = parser.parseArgs(args1a)
    checkArgs(expected1, expectedPos1, parsed)
  }

  // scenario 1b: all args present explicitly, along with some interspersed extras,
  //              and 'FALSE' is passed for '-d'
  val args1b = Array("pos1", "--cArg", "42", "pos2", "pos3", "-a", "aValue", "-d", "FALSE", "--multiArg", "m1", "m2", "m3", "-b", "bVal", "pos4")
  val expected1b = expected1 + (argD -> false)

  def testScenario1b(parser: ArgParser) {
    val parsed = parser.parseArgs(args1b)
    checkArgs(expected1b, expectedPos1, parsed)
  }

  // scenario 1c: all args present explicitly, and "equals" syntax is used (e.g. --foo=bar)
  val args1c = Array("pos1", "--cArg=42", "pos2", "pos3", "-a=aValue", "-d", "FALSE", "--multiArg", "m1", "m2", "m3", "-b", "bVal", "pos4")

  def testScenario1c(parser: ArgParser) {
    val parsed = parser.parseArgs(args1c)
    checkArgs(expected1b, expectedPos1, parsed)
  }

  // scenario 2: -d missing (should default to false), -b missing (should default to "")
  val args2 = Array("pos1", "--cArg", "42", "pos2", "pos3", "-a", "aValue", "pos4", "--multiArg", "m1", "m2", "m3")
  val expected2 = expected1 ++ List(argB -> "", argD -> false)

  def testScenario2(parser: ArgParser) {
    val parsed = parser.parseArgs(args2)
    checkArgs(expected2, expectedPos1, parsed)

  }

  // scenario 3: no args...lots of problems with this one
  def testScenario3(parser: ArgParser) {
    shouldThrowIllegalArg("scenario 3", parser.parseArgs(Array[String]()))
  }

  // scenario 4: missing a value for '-a', which is required
  val args4 = Array("pos1", "--cArg", "42", "pos2", "pos3", "-d", "--multiArg", "m1", "m2", "m3", "-b", "bVal", "pos4")
  def testScenario4(parser: ArgParser) {
    shouldThrowIllegalArg("scenario 4", parser.parseArgs(args4))
  }

  // scenario 5: pass a non-numerical value for -c, which is an IntArg
  val args5 = Array("pos1", "--cArg", "notANumber", "pos2", "pos3", "-a", "aValue", "-d", "--multiArg", "m1", "m2", "m3", "-b", "bVal", "pos4")
  def testScenario5(parser: ArgParser) {
    shouldThrowIllegalArg("scenario 5", parser.parseArgs(args5))
  }

  // scenario 6: pass a non-bool value for -d
  val args6 = Array("pos1", "--cArg", "42", "pos2", "pos3", "-a", "aValue", "-d", "trueOrFalse", "--multiArg", "m1", "m2", "m3", "-b", "bVal", "pos4")
  def testScenario6(parser: ArgParser) {
    shouldThrowIllegalArg("scenario 6", parser.parseArgs(args6))
  }

  def checkArgs(expectedMap: Map[ArgSpec[_], Any], expectedPositionals: List[String], parsedArgs: ParsedArgs) {
    expectedMap.foreach {
      case (arg, expected) =>
        val found = parsedArgs.get(arg)
        assert(expected == found, "for arg %s, expected %s but found %s".format(arg, expected, found))
    }
    assert(expectedPositionals == parsedArgs.positionalArgs,
      "expected positional arguments, %s, but found %s".format(expectedPositionals, parsedArgs.positionalArgs))
  }

  def shouldThrowIllegalArg(msg: String, todo: => Unit) {
    try {
      todo
      throw new AssertionError("%s: expected an IllegalArgumentException...nothing happened".format(msg))
    } catch  {
      case _: IllegalArgumentException => ()
    }
  }

  def main(args: Array[String]) {
    val parser1 = ArgParser(argA, argB, argC, argD, argMulti)
    val parser2 = new ArgParser(Set(argA, argB, argC, argD, argMulti))
    val parser3 = ArgParser().addArg(argA).addArg(argB).addArg(argC).addArg(argD).addArg(argMulti)
    val parser4 = ArgParser(argA, argB, argC).addArg(argD).addArg(argMulti)

    val parsers = List(parser1, parser2, parser3, parser4)
    for (parser <- parsers) {
      testScenario1(parser)
      testScenario1a(parser)
      testScenario1b(parser)
      testScenario1c(parser)
      testScenario2(parser)
      testScenario3(parser)
      testScenario4(parser)
      testScenario5(parser)
      testScenario6(parser)
    }
    println("(all tests passed)")
  }
}

object BasicParserDemo {
  val argA = new StringArg(Set("--a", "-a"))
  val argB = new StringArg(Set("--bArg", "-b"))
  val argMulti = new MultiStringArg(Set("--multiArg", "-m"))
  def main(args: Array[String]) {
    val parser = ArgParser(argA, argB, argMulti)
    val results = parser.parseArgs(args)
    println("parsed...")
    println("positional args: " + results.positionalArgs.toList.mkString(", "))
    // println("arges w/values: " + handler.argesWithRawValues.mkString(", "))
    val argAVal: String = results.get(argA)
    val argBVal: String = results.get(argB)
    val argMultiVals: List[String] = results.get(argMulti)
    println("argA: " + argAVal)
    println("argB: " + argBVal)
    println("argMulti: " + argMultiVals)
  }
}
