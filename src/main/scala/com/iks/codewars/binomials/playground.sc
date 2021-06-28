import com.iks.codewars.binomials.BinomialExpansion._

expand("(2x-4)^2")     // returns "4x^2-16x+16"
expand("(x+1)^2")      // returns "x^2+2x+1"
expand("(p-1)^3")      // returns "p^3-3p^2+3p-1"
expand("(2f+4)^6")     // returns "64f^6+768f^5+3840f^4+10240f^3+15360f^2+12288f+4096"
expand("(-2a-4)^0")    // returns "1"
expand("(-12t+43)^2")  // returns "144t^2-1032t+1849"
expand("(r+0)^203")    // returns "r^203"
expand("(-x-1)^2")     // returns "x^2+2x+1"

expand("(x+1)^1")
expand("(x-1)^1")

expand("(y+5)^15")
expand("(47B+81)^10")
expand("(-3n-90)^10")
expand("(23S-90)^10")
expand("(-75c+1)^9")
expand("(83F+65)^10")
expand("(-68c-39)^10")
val a = 83
val b = 65
val n = 10
BigInt(83).pow(10)
