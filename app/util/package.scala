package object util {

  import play.api.libs.iteratee._
  import play.api.Logger

  type AB = Array[Byte]

  def enumTap0[E: Show](id: String): Enumeratee[E, E] = Enumeratee.mapInput {
    case e =>
      Logger.debug("enumTap[%s]: %s" format (id, implicitly[Show[E]].strEl(e)))
      e
  }

  def loggedFold[E: Show, A: Show, B, C](log: String => Unit, i: Iteratee[E, A])(
    done: (A, Input[E]) => Iteratee[B, C],
    cont: ((Input[E]) => Iteratee[E, A]) => Iteratee[B, C],
    error: (String, Input[E]) => Iteratee[B, C]): Iteratee[B, C] = {
    val show = implicitly[Show[E]]
    i.pureFlatFold(
      done = (a, e) => {
        log("Done(%s, %s)" format (implicitly[Show[A]].str(a), show.strEl(e)))
        done(a, e)
      },
      cont = k => {
        def logged(e: Input[E]) = {
          log("Cont " + show.strEl(e))
          k(e)
        }
        cont(logged)
      },
      error = (msg, e) => {
        log("Error(%s, %s)" format (msg, show.strEl(e)))
        error(msg, e)
      })
  }
  
  def enumTap[E: Show](id: String): Enumeratee[E, E] = new Enumeratee[E, E] {
    def log(msg: String) { Logger.debug("enumTap[%s]: %s" format(id, msg)) }
    type IE[A] = Iteratee[E, A]
    def checkDoneLogged[A](i: IE[A])(continue: K[E, A] => Iteratee[E, IE[A]]): Iteratee[E, IE[A]] = {
      loggedFold(log, i)(
          (a, e) => Done(i, e),
          continue,
          (msg, e) => Error(msg, e))
    }
    def applyOn[A](inner: Iteratee[E, A]): Iteratee[E, Iteratee[E, A]] = {
      def step(e: Input[E], i: IE[A]): Iteratee[E, IE[A]] = {
        checkDoneLogged[A](i) { k =>
          val i2 = k(e)
          checkDoneLogged[A](i2) { _ => Cont(step(_, i2)) }
        }
      }
      Cont(step(_, inner))
    }
  }

  def iterTap(id: String) = new IterTap(id)

  class IterTap(id: String) {
    def ?<<[E: Show, A: Show](inner: Iteratee[E, A]): Iteratee[E, A] = {
      def log(msg: String) = Logger.debug("iterTap[%s]: %s" format (id, msg))
      loggedFold(log, inner)(
        (a, e) => inner,
        k => Cont((e: Input[E]) => iterTap(id) ?<< k(e)),
        (msg, e) => inner)
    }
  }
  
}