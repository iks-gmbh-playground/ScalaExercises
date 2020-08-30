object Meeting {
  def meeting(s: String): String = {
    s.split(";")
      .map(_.split(":")
        .reverse
        .map(_.toUpperCase)
        .mkString("(", ", ", ")"))
      .sorted
      .mkString
  }
}

object MeetingMain extends App {

  import Meeting.meeting

  val s = "Fred:Corwill;Wilfred:Corwill;Barney:Tornbull;Betty:Tornbull;Bjon:Tornbull;Raphael:Corwill;Alfred:Corwill"

  println(s)
  println(meeting(s))
}
