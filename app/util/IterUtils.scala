package util

import play.api.libs.iteratee._
import play.api.Logger

trait Show[E] {
  def str(e: E): String
  def strEl(e: Input[E]): String = e match {
    case Input.Empty => "Empty"
    case Input.EOF => "EOF"
    case Input.El(ee) => "Input.El(%s)" format str(ee)
  }
}

object Show {

  implicit val showAB: Show[AB] = new Show[AB] { def str(ab: AB) = new String(ab) }
  
  implicit def showArray[A: Show]: Show[Array[A]] = new Show[Array[A]] {
    def str(seq: Array[A]) = {
      val sh = implicitly[Show[A]]
      seq.map(sh.str(_)).mkString("[", ", ", "]")
    }
  }
  
  implicit def showAny[T <: Any]: Show[T] = new Show[T] { def str(any: T) = any.toString }
  
  implicit def showTuple[A: Show, B: Show]: Show[(A, B)] = new Show[(A, B)] {
    def str(t: (A, B)) = "(%s,%s)" format (implicitly[Show[A]].str(t._1), implicitly[Show[B]].str(t._2))
  }

  implicit def showSeq[A: Show, CC[_] <: Seq[A]]: Show[CC[A]] = new Show[CC[A]] {
    def str(seq: CC[A]) = {
      val sh = implicitly[Show[A]]
      seq.map(sh.str(_)).mkString("[", ", ", "]")
    }
  }

}
