package wdf.argParse

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.{ Map => MutableMap }

class ArgParser(argSpecs: Iterable[ArgSpec[_]]) {
  import ArgParser._

  val switchMap = buildSwitchMap(argSpecs)

  def addArg(argSpec: ArgSpec[_]): ArgParser = new ArgParser(argSpec :: argSpecs.toList)

  def parseArgs(argsToParse: Array[String]): ParsedArgs = parseArgs(argsToParse.toList)

  def parseArgs(argsToParse: List[String],
    positionalArgsRev: List[String] = List(),
    argSpecsWithRawValues: Map[ArgSpec[_], List[String]] = Map()
  ): ParsedArgs = argsToParse match {
    case Nil =>
      validateArgs(argSpecsWithRawValues, switchMap.values.toSet)
      new ParsedArgs(positionalArgsRev.reverse, argSpecsWithRawValues)
    case firstArg :: rest =>
      if (isSwitch(firstArg)) {
        parseSwitchAndMaybeValue(firstArg) match {
          case (argSpec, Some(value)) =>
            parseArgs(rest, positionalArgsRev, argSpecsWithRawValues + (argSpec -> List(value)))
          case (argSpec, None) =>
            val (remaining, values) = collectArgSpecValues(rest, argSpec.isMultiValued)
            parseArgs(remaining, positionalArgsRev, argSpecsWithRawValues + (argSpec -> values))
        }
      } else {
        parseArgs(rest, firstArg :: positionalArgsRev, argSpecsWithRawValues)
      }
  }

  private def validateArgs(argSpecsWithRawValues: Map[ArgSpec[_], List[String]],
    allArgSpeces: Iterable[ArgSpec[_]]) {
      allArgSpeces.foreach {
        argSpec => argSpec.get(argSpecsWithRawValues.get(argSpec))
      }
    }

  private def getArgSpecForSwitch(switch: String) = switchMap.get(switch) match {
    case Some(switch) => switch
    case _ => throw new IllegalArgumentException("unrecognized switch: " + switch)
  }

  private val equalsAssignmentPattern = """^([^=]+)=(.+)$""".r
  private def parseSwitchAndMaybeValue(arg: String): (ArgSpec[_], Option[String]) = arg match {
    case equalsAssignmentPattern(lhs, rhs) => (getArgSpecForSwitch(lhs), Some(rhs))
    case _ => (getArgSpecForSwitch(arg), None)
  }
}

object ArgParser {
  def buildSwitchMap(argSpecs: Iterable[ArgSpec[_]]): Map[String, ArgSpec[_]] =
    (Map[String, ArgSpec[_]]() /: argSpecs) {
      case (acc, argSpec) =>
        assert(!argSpec.switches.exists(acc.contains), "duplicate switches detected!")
        acc ++ argSpec.switches.map(_ -> argSpec)
    }

  def isSwitch(arg: String): Boolean = arg.startsWith("-")

  /**
   * for use when we've just found a switch, and now need to collect all
   * of the values associated with that switch (where a 'value' is any arg
   * which isn't a switch itself)
   *
   * return (remaining args, values collected)
   */
  def collectArgSpecValues(args: List[String], isMultiValued: Boolean): (List[String], List[String]) = {
    def helper(remainingArgs: List[String], accRev: List[String]=Nil): (List[String], List[String]) =
      remainingArgs match {
        case Nil => (Nil, accRev.reverse)
        case firstArg :: rest =>
          if (!isSwitch(firstArg) && (accRev.isEmpty || isMultiValued)) {
            helper(rest, firstArg :: accRev)
          } else {
            (remainingArgs, accRev.reverse)
          }
      }
    helper(args)
  }
  def apply(argSpecs: ArgSpec[_]*) = new ArgParser(argSpecs.toSet)
}

class ParsedArgs(
  val positionalArgs: List[String],
  val argSpecsWithRawValues: Map[ArgSpec[_], List[String]]
) {
  def isPresent(argSpec: ArgSpec[_]): Boolean = argSpecsWithRawValues.get(argSpec).isDefined
  def tryGet[T](argSpec: ArgSpec[T]): Option[T] =
    if (isPresent(argSpec)) Some(get(argSpec)) else None
  def get[T](argSpec: ArgSpec[T]): T = argSpec.get(argSpecsWithRawValues.get(argSpec))
  def get(arg: String): Option[String] = getMulti(arg) match {
    case None => None
    case Some(onlyValue :: Nil) => Some(onlyValue)
    case Some(multipleValues) => throw new IllegalArgumentException("called single-valued 'get' for argument, '%s', which is multi-valued (values: %s). try 'getMulti'.".format(arg, multipleValues.mkString(",")))
  }
  def getMulti(arg: String): Option[List[String]] = argSpecsWithRawValues.find(_._1.switches.contains(arg)) match {
    case None => None
    case Some((_, rawValues)) => Some(rawValues)
  }
}
