import org.scalatest._
import org.scalatest.matchers.should.Matchers

import Monads._

class MonadsSpec extends PropSpec with Matchers {
  def leftASSO[M[_], A, B, C](amb: A => M[B])(bmc: B => M[C])(ma: M[A])(implicit m: Monad[M]): M[C] = m.bind(m.bind(ma)(amb))(bmc)
  def rightASSO[M[_], A, B, C](amb: A => M[B])(bmc: B => M[C])(ma: M[A])(implicit m: Monad[M]): M[C] = m.bind(ma)((a: A) => m.bind(amb(a))(bmc))
  def leftID[M[_], A, B](a: A)(amb: A => M[B])(implicit m: Monad[M]): M[B] = m.bind(m.unit(a))(amb)
  def rightID[M[_], A](ma: M[A])(implicit m: Monad[M]): M[A] = m.bind(ma)(m.unit)

  property("State") {
    def m = implicitly[Monad[({type x[a]=State[Int, a]})#x]]
    def f(x: Int) = State((s: Int) => (s - 1, x + 1))
    def g(x: Int) = m.unit(x+1)
    leftID(5)(f).run(1) should be (f(5).run(1))
    rightID(f(5)).run(1) should be (f(5).run(1))
    leftASSO(f)(g)(m.unit(5)).run(1) should be (rightASSO(f)(g)(m.unit(5)).run(1))
  }

  property("Reader") {
    def m = implicitly[Monad[({type x[a]=Reader[Int, a]})#x]]
    def f(x: Int) = Reader((r: Int) => r + x)
    def g(x: Int) = m.unit(x+1)
    leftID(5)(f).run(1) should be (f(5).run(1))
    rightID(f(5)).run(1) should be (f(5).run(1))
    leftASSO(f)(g)(m.unit(5)).run(1) should be (rightASSO(f)(g)(m.unit(5)).run(1))
  }

  property("Writer") {
    implicit val intMonoid: Monoid[Int] = new Monoid[Int] {
      def mempty: Int = 0
      def mappend(x: Int)(y: Int): Int = x + y
    }
    def rightID[M[_], A](ma: M[A])(implicit m: Monad[M]): M[A] = m.bind(ma)(m.unit)

    def m: Monad[({
      type x[a] = Writer[Int, a]
    })#x] = implicitly[Monad[({type x[a]=Writer[Int, a]})#x]]

    def f(x: Int) = Writer((x+1, x))
    def g(x: Int): Writer[Int, Int] = m.unit(x)

    leftID(5)(f) should be (f(5))
    rightID(f(5)) should be (f(5))
    leftASSO(f)(g)(m.unit(5)) should be (rightASSO(f)(g)(m.unit(5)))
  }
}