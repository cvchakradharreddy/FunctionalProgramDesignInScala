package calculator

import scala.math.{pow, sqrt}

object Polynomial extends PolynomialInterface {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = {
    Signal(pow(b(), 2) - (4 * a() * c()))
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      delta() match {
        case d if d < 0 => Set()
        case _ => Set((((-1 * b()) + sqrt(delta()))) / (2 * a()), (((-1 * b()) - sqrt(delta()))) / (2 * a()))
      }
    }
  }

}
