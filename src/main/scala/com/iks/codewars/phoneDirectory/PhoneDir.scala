package com.iks.codewars.phoneDirectory

import scala.util.matching.Regex

// John keeps a backup of his old personal phone book as a text file. On each line of the file he can find
// the phone number (formated as +X-abc-def-ghij where X stands for one or two digits), the corresponding
// name between < and > and the address.
//
// Unfortunately everything is mixed, things are not always in the same order; parts of lines are
// cluttered with non-alpha-numeric characters (except inside phone number and name).
//
// Examples of John's phone book lines:
//
// "/+1-541-754-3010 156 Alphand_St. <J Steeve>\n"
//
// " 133, Green, Rd. <E Kustur> NY-56423 ;+1-541-914-3010!\n"
//
// "<Anastasia> +48-421-674-8974 Via Quirinal Roma\n"
//
// Could you help John with a program that, given the lines of his phone book and a phone number num
// returns a string for this number : "Phone => num, Name => name, Address => address"
//
// Examples:
//
// s = "/+1-541-754-3010 156 Alphand_St. <J Steeve>\n 133, Green, Rd. <E Kustur> NY-56423 ;+1-541-914-3010!\n"
//
// phone(s, "1-541-754-3010") should return "Phone => 1-541-754-3010, Name => J Steeve, Address => 156 Alphand St."
// It can happen that there are many people for a phone number num, then
//
// return : "Error => Too many people: num"
//
// or it can happen that the number num is not in the phone book, in that case
//
// return: "Error => Not found: num"

object PhoneDir {
  val phonePattern: Regex = """\W*?(\d{1,2}(-\d+){3})\W*?""".r
  val namePattern: Regex = """<.*?>""".r

  def formatLine(line: String, num: String): String = {
    val name = namePattern.findFirstIn(line).get.tail.init
    val address = phonePattern.replaceAllIn(namePattern.replaceAllIn(line, ""), "").
      replaceAll("""([^\w .-])*""", "").
      replaceAll("_", " ").
      replaceAll(" {2,}", " ").trim
    s"Phone => $num, Name => $name, Address => $address"
  }

  def phoneHto(dir: String, num: String): String =
    dir.split("\n").filter(phonePattern.findFirstMatchIn(_).exists(_.group(1) == num)) match {
      case ls if ls.length == 0 => s"Error => Not found: $num"
      case ls if ls.length > 1 => s"Error => Too many people: $num"
      case Array(line) => formatLine(line, num)
    }

  def phone(str: String, num: String): String = {
    (".*\\+" + num + ".*").r.findAllIn(str).map(x=>x.toString).toArray match {
      case lines if lines.isEmpty => s"Error => Not found: $num"
      case lines if lines.length > 1 => s"Error => Too many people: $num"
      case Array(line) =>
        val name = "<.*?>".r.findFirstIn(line).get.tail.init
        val address = line.
          split(s"$num|<.*?>").mkString.
          replaceAll("[^\\w.\\-\\s]", " ").
          replaceAll(" +", " ").
          trim()
        s"Phone => $num, Name => $name, Address => $address"
    }
  }
}
