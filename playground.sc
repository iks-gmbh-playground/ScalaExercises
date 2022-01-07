val zeile10 = 2
var zeile11 = 0
var zeile12 = 1000
val zeile13 = 1
var zeile14 = -1
var zeile15 = -1

var run = true
while (run) {
  zeile11 = zeile11 + zeile10
  if (zeile11 == zeile12) {
    zeile15 = zeile11
    run = false
  } else {
    zeile12 = zeile12 - zeile13
    if (zeile12 < zeile11) {
      zeile14 = zeile12
      run = false
    }
  }
}

println(s"zeile14 = $zeile14")
println(s"zeile15 = $zeile15")