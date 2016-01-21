# scala-arg-parser
A couple of super simple command line argument parsing utilities for Scala.
Example:

    def main(args: Array[String]) {
      // first, define specs for each of the CL arguments you want to parse
      val optionalStringArg = StringArg(Set("--optString", "-o"))
      val requiredStringArg = StringArg(Set("--reqString", "-r"), required=true)
      val intArg = IntArg(Set("--int", "i"))
      val boolArg = BoolArg(Set("--bool", "-b"))

      // next, create an ArgParser instance and pass the CL args array to 'parseArgs',
      // which returns a ParsedArgs instance containing the parse results
      val parser = ArgParser(optionalStringArg, requiredStringArg, intArg, boolArg)
      val parsedArgs = parser.parseArgs(args)

      // at this point, you can retrieve the arg value for each arg spec from above
      val optionalStringArgVal: String = parsedArgs.get(optionalStringArg)
      val requiredStringArgVal: String = parsedArgs.get(requiredStringArg)
      val intArgVal: Int = parsedArgs.get(intArg)
      val boolArgVal: Boolean = parsedArgs.get(boolArg)

      // the nice thing about get(ArgSpec) is that its return value is strongly
      // typed, but you can also use get(String) or getMulti(String) to retrieve
      // the raw string value(s) passed for a given argument in the command line
      val intArgValAsString: String = parsedArgs.get("--int")
      val boolArgValAsString: String = parsedArgs.get("-b")
    }
