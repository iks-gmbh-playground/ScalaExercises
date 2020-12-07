import org.scalatest.flatspec.AnyFlatSpec

class MorseDecoderTest extends AnyFlatSpec {
  import MorseDecoder.decode

  "the example from the description" should "return \"HEY JUDE\"" in {
    assertResult("HEY JUDE")(decode(".... . -.--   .--- ..- -.. ."))
  }

}
