package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = Signal {
    val bVal = b()
    bVal * bVal - 4 * a() * c()
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = Signal {
    val Δ = computeDelta(a,b,c)()
    if(Δ < 0) {
      Set[Double]()
    } else {
      val sqrtΔ = Math.sqrt(Δ)
      val bVal = b()
      val twoAVal = 2 * a()
      Set(
        (- bVal + sqrtΔ) / twoAVal,
        (- bVal - sqrtΔ) / twoAVal
      )
    }
  }
}
