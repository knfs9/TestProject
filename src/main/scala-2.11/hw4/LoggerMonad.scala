package hw4

object LoggerMonad {

  trait Monoid[A] {
    def append(a1: A, a2: A): A

    def empty: A
  }

  object MonoidUtils {
    implicit def ListMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
      def append(a1: List[A], a2: List[A])= a1 ::: a2
      def empty = Nil
    }
  }

  case class Logger[LOG, A] (log: LOG, value: A) {
    def map[B] (f: A => B) = Logger(log, f(value))

    def flatMap[B](f: A => Logger[LOG, B]) (implicit m: Monoid[LOG]) = {
      val x = f(value)
      Logger(m.append(log, x.log), x.value)
    }
  }

  object Logger {
    def unital[LOG, A] (value: A) (implicit m: Monoid[LOG]) = Logger(m.empty, value)
  }

  object Util {
    implicit def ListLogUtil[A] (a: A) = new {
      def ~> [B] (b: B) = Logger(List(a + s" [TIMESTAMP: ] ${System.currentTimeMillis()}"), b)
      def <|~ [B] (k: A => B) = Logger(List(k(a)), a)
    }
  }
}