import scala.collection.mutable

def swap(buffer: mutable.ArrayBuffer[String], index: Int): mutable.ArrayBuffer[String] = {
  val temp = buffer(index + 1)
  buffer(index + 1) = buffer(index)
  buffer(index) = temp
  buffer
}

def arrange(s: String): String = {
  var words = mutable.ArrayBuffer(s.split(" +"): _*)
  for (i <- words.indices.init) {
    val iIsEven = i % 2 == 0
    val iIsOdd = i % 2 == 1
    val lenWordA = words(i).length
    val lenWordB = words(i + 1).length
    words = if ((iIsEven && lenWordA > lenWordB) ||
      (iIsOdd && lenWordA < lenWordB)) swap(words, i)
    else words
  }
  words.indices.map(i => if (i % 2 == 0) words(i).toLowerCase else words(i).toUpperCase).mkString(" ")
}

val text = "who hit retaining The That a we taken"
arrange(text)

var words = mutable.ArrayBuffer(text.split(" +"): _*)

def switch(i: Int, j: Int): String = {
  val temp = words(j)
  words(j) = words(i)
  words(i) = temp
  temp
}

words.indices.map( i => {
  if (i % 2 == 0)
    (if (i < words.length - 1 && words(i).length > words(i+1).length) switch(i, i + 1)
    else words(i)).toLowerCase
  else
    (if (i < words.length - 1 && words(i).length < words(i+1).length) switch(i, i + 1)
    else words(i)).toUpperCase
})

words
words.patchInPlace(1, "one", 2)