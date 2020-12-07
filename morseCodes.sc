import MorseDecoder._
import MorseDecoderAdvanced._

import scala.util.matching.Regex

val end: Regex = "(0*)$".r
val ones: Regex = "(1+).*".r
val zeros: Regex = "(0+).*".r

val bits = "1100110011001100000011000000111111001100111111001111110000000000000011001111110011111100111111000000110011001111110000001111110011001100000011"
val msg = ".... . -.--   .--- ..- -.. ."

val result = decodeBits(bits)
result == msg
