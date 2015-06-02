package calculator

import scala.math.{sqrt, pow}

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] =
    Signal {
      pow(b(), 2) - 4 * a() * c()
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] =
    Signal {
      val deltav = delta()
      deltav match {
        case v: Double if v <  0 => Set()
        case v: Double if v >= 0 => {
          val av = a()
          val bv = b()
          val sqd = sqrt(deltav)
          Set((sqd - bv)/(2 * av), (-sqd - bv)/(2 * av))
        }
      }
  }
}
