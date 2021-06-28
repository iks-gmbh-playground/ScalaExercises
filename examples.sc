
val v = "123"
val i = if (v.matches("\\d+")) v.toInt else 0

val regs = Map("a" -> 1, "b" -> 2, "c" -> 3)
regs("a")
regs + ("d" -> regs("a"))
regs + ("d" -> regs("b"))