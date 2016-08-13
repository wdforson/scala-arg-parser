package wdf.argParse

trait ArgSpec[T] {
  def switches: Set[String]
  def required: Boolean = false
  def isMultiValued: Boolean = false

  def get(rawValuesOpt: Option[List[String]]): T
  // NOTE: this can be extended to include custom validation logic
  //       (this method will be called for all registered ArgSpecs
  //        before ArgParser.parseArgs returns)
  def isValid(rawValuesOpt: Option[List[String]]): Boolean =
    !required || rawValuesOpt.getOrElse(Nil).nonEmpty

  override def toString(): String = "{ %s, %s, %s }".format(
    switches.mkString(", "), if (required) "required" else "optional",
      if (isMultiValued) "repeating" else "non-repeating")
}

object ArgSpec {
  def stringArg(switches: String*) = StringArg(switches.toSet)
  def requiredStringArg(switches: String*) = StringArg(switches.toSet, required=true)

  def boolArg(switches: String*) = BoolArg(switches.toSet)
  def requiredBoolArg(switches: String*) = BoolArg(switches.toSet, required=true)

  def intArg(switches: String*) = IntArg(switches.toSet)
  def requiredIntArg(switches: String*) = IntArg(switches.toSet, required=true)

  def longArg(switches: String*) = LongArg(switches.toSet)
  def requiredLongArg(switches: String*) = LongArg(switches.toSet, required=true)

  def multiStringArg(switches: String*) = MultiStringArg(switches.toSet)
  def requiredMultiStringArg(switches: String*) = MultiStringArg(switches.toSet, required=true)
}

abstract class BaseArgSpec[T](switches: Set[String])
extends ArgSpec[T] {
  private val switchRegex = """^\-{1,2}[a-zA-Z_]+$""".r
  private def isValid(switch: String): Boolean = switch match {
    case switchRegex() => true
    case _ => false
  }
  private def validateSwitches() {
    val invalidSwitches = switches.filterNot(isValid)
    if (invalidSwitches.nonEmpty) {
      throw new IllegalArgumentException("the following switches do not match the pattern, %s: %s".format(switchRegex, invalidSwitches.mkString(" ")))
    }
  }
  validateSwitches()
}

case class StringArg(
  switches: Set[String],
  override val required: Boolean = false,
  defaultIfOptional: String = ""
) extends BaseArgSpec[String](switches) {

  def get(rawValuesOpt: Option[List[String]]): String = rawValuesOpt match {
    case Some(only :: Nil) => only
    case None | Some(Nil) =>
      if (!required) defaultIfOptional
      else throw new IllegalArgumentException("switch %s requires a value, none given".format(this))
    case Some(multi) => throw new IllegalArgumentException("switch %s only takes on value. got: %s".format(this, multi.mkString(",")))
  }
}

case class BoolArg(
  switches: Set[String],
  override val required: Boolean = false,
  defaultIfOptional: Boolean = false
) extends BaseArgSpec[Boolean](switches) {
  def get(rawValuesOpt: Option[List[String]]): Boolean = rawValuesOpt match {
    case None => defaultIfOptional
    case Some(Nil) => true
    case Some(first :: _) => first.toBoolean
  }
}

case class IntArg(
  switches: Set[String],
  override val required: Boolean = false,
  defaultIfOptional: Int = 0
) extends BaseArgSpec[Int](switches) {
  def get(rawValuesOpt: Option[List[String]]): Int = rawValuesOpt match {
    case Some(first :: _) => first.toInt
    case None | Some(Nil) =>
      if (!required) defaultIfOptional
      else throw new IllegalArgumentException("switch %s requires a value, none given".format(this))
  }
}

case class LongArg(
  switches: Set[String],
  override val required: Boolean = false,
  defaultIfOptional: Long = 0
) extends BaseArgSpec[Long](switches) {
  def get(rawValuesOpt: Option[List[String]]): Long = rawValuesOpt match {
    case Some(first :: _) => first.toLong
    case None | Some(Nil) =>
      if (!required) defaultIfOptional
      else throw new IllegalArgumentException("switch %s requires a value, none given".format(this))
  }
}

case class MultiStringArg(
  switches: Set[String],
  override val required: Boolean = false,
  defaultIfOptional: List[String] = List()
) extends BaseArgSpec[List[String]](switches) {

  override val isMultiValued = true
  def get(rawValuesOpt: Option[List[String]]): List[String] = rawValuesOpt match {
    case None | Some(Nil) =>
      if (!required) defaultIfOptional
      else throw new IllegalArgumentException("switch %s requires at least one value, none given".format(this))
    case Some(rawValues) => rawValues
  }
}
