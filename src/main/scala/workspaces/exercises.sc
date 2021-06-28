val StartWithUppercase = "([A-Z].*)".r

StartWithUppercase.matches("Hartwig")

val word = "Hartwig"

val result = word match {
  case StartWithUppercase(c) => s"Yes, $c starts with an uppercase."
  case _ => "Sorry no uppercase in position one."
}

def parseMessageToString(msg: String): String = {
  val StartsWithString = "('.+?')(,\\s(.*))?".r("text", "tail")
  val StartsWithInt = "(\\d+)(,\\s(.*))?".r("int", "tail")
  val StartsWithName = "([A-Za-z]+\\S*)(,\\s(.*))?".r("name", "tail")
  msg match {
    case StartsWithString(text, _, tail) => s"found text first: $text${if (tail != null) s" and rest: $tail" else ""}"
    case StartsWithInt(int, _, tail) => s"found Int first: $int${if (tail != null) s" and rest: $tail" else ""}"
    case StartsWithName(reg, _, tail) => s"found register name first: $reg${if (tail != null) s" and rest: $tail" else ""}"
  }
}

def parseMessageToOptionTuple(msg: String): (Option[String], Option[String], Option[String], Option[String]) = {
  val StartsWithString = "('.+?')(,\\s(.*))?".r
  val StartsWithInt = "(\\d+)(,\\s(.*))?".r
  val StartsWithName = "([A-Za-z]+\\S*)(,\\s(.*))?".r
  msg match {
    case StartsWithString(text, _, tail) => (Some(text), None, None, Option(tail))
    case StartsWithInt(int, _, tail) => (None, Some(int), None, Option(tail))
    case StartsWithName(reg, _, tail) => (None, None, Some(reg), Option(tail))
  }
}

def parseMessage(msg: String): Map[String, String] = {
  val StartsWithString = "('.+?')(,\\s(.*))?".r
  val StartsWithInt = "(\\d+)(,\\s(.*))?".r
  val StartsWithName = "([A-Za-z]+\\S*)(,\\s(.*))?".r
  msg match {
    case StartsWithString(text, _, tail) =>
      if (tail == null) Map("text" -> text)
      else Map("text" -> text, "tail" -> tail)
    case StartsWithInt(int, _, tail) =>
      if (tail == null) Map("int" -> int)
      else Map("int" -> int, "tail" -> tail)
    case StartsWithName(reg, _, tail) =>
      if (tail == null) Map("register" -> reg)
      else Map("register" -> reg, "tail" -> tail)
    case _ => Map()
  }
}

object MessagePart extends Enumeration {
  val Text, Number, Register, Invalid = Value
}

def parseMessageOption(msg: String): (MessagePart.Value, String, Option[String]) = {
  import MessagePart._

  val StartsWithString = "('.+?')(,\\s(.*))?".r
  val StartsWithInt = "(\\d+)(,\\s(.*))?".r
  val StartsWithName = "([A-Za-z]+\\S*)(,\\s(.*))?".r
  msg match {
    case StartsWithString(text, _, tail) => (Text, text, Option(tail))
    case StartsWithInt(number, _, tail) => (Number, number, Option(tail))
    case StartsWithName(reg, _, tail) =>(Register, reg, Option(tail))
    case _ => (Invalid, "", None)
  }
}

val message1 = "'this is a text ', 54, ' and another text'"
val message1a = "'this is a text '"
parseMessageOption(message1)
parseMessageOption(message1a)

val message2 = "54, ' and another text'"
val message2a = "54"
parseMessageOption(message2)
parseMessageOption(message2a)

val message3 = "Reg54, ' and another text'"
val message3a = "Reg54"
parseMessageOption(message3)
parseMessageOption(message3a)
