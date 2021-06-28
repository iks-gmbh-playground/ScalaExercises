import com.iks.codewars.caesarsCipher.FirstVariation._

val s = "The quick brown fox jumped over the lazy german shepard!"

val sEncoded = movingShift(s, 1)
val sDecoded = demovingShift(sEncoded, 1)

val u2 = "O CAPTAIN! my Captain! our fearful trip is done;";
val u2Enc = movingShift(u2, 5)
val v2 = List("T JIYDLUA!", " cp Vukpxg", "m! qxv lli", "apfx hgyg ","bm zlld;");
val v2Dec = demovingShift(u2Enc, 5)

val u1 = "I should have known that you would have a perfect answer for me!!!";
val u1Enc = movingShift(u1, 1)
val v1 = List("J vltasl rlhr ", "zdfog odxr ypw", " atasl rlhr p ", "gwkzzyq zntyhv", " lvz wp!!!");
val v1Dec = demovingShift(v1, 1)

-52 % 26