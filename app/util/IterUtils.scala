package util

import play.api.libs.iteratee._
import play.api.Logger

/**
 * Machinery to print out Inputs.
 */
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

/**
 * Small example using flatMap and map to combine iteratees.
 */
object PNG {
  
  val show = implicitly[Show[AB]]

  def get(n: Int) = Traversable.take[AB](n) &>> Iteratee.consume[AB]()
  
  val getInt: Iteratee[AB, Int] = get(4).map{ arr => 
    arr(0)<<24 | arr(1)<<16 | arr(2)<<8 | arr(3)
  }

  val PNG_HDR = Array(0x89, 'P', 'N', 'G', '\r', '\n', 0x1A, '\n').map(_.toByte)

  val header = get(8).flatMap { arr =>
    if (arr.sameElements(PNG_HDR)) Done(true, Input.Empty)
    else Error("invalid PNG header", Input.EOF)
  }

  val chunkType: Iteratee[AB, String] = get(4).map(show.str)

  def chunkCRC = getInt

  def chunk = for {
    length <- getInt
    tpe <- chunkType
    data <- get(length)
    crc <- getInt
  } yield {
    (length, tpe)
  }

  def png = for {
    h <- header
    chunks <- Iteratee.repeat(chunk)
  } yield {
    chunks
  }
  
  def read(url: String) = {
    val in = new java.net.URL(url).openStream()
    val source = Enumerator.fromStream(in, 1024)
    val promise = Iteratee.flatten(source |>> png).run
    val result = promise.await.get
    result
  }
}
  

