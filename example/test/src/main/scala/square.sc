def abs(x: Double) = if (x < 0) -x else x

def sqrt(x: Double) = {
  def isGoodEnough(guess: Double) = abs(guess * guess - x) / x <= 0.000001

  def improve(guess: Double) = (guess + x / guess) / 2

  def sqrtIter(guess: Double, x: Double): Double =
    if (isGoodEnough(guess)) guess
    else sqrtIter(improve(guess), x)

  sqrtIter(1.0, x)
}

sqrt(1.0e50)