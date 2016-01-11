package aoptest

object AOPTest2 extends App {
  trait Stuff {
    def doStuff(x: String): String
  }

  trait LoggableStuff extends Stuff {
    abstract override def doStuff(x: String): String = {
      println("doStuff method was invoked at! [LoggableStuff]")
      super.doStuff(x)
    }
  }

  trait TransactionalStuff extends Stuff {
    abstract override def doStuff(x: String): String = {
      try {
        println("Transaction begins! [TransactionalStuff]")
        val result = super.doStuff(x)
        println("Transaction completed! [TransactionalStuff]")
        result
      } catch {
        case e: Exception =>
          println("Transaction rollback! [TransactionalStuff]")
          ""
      }
    }
  }

  class RealStuff extends Stuff {
    override def doStuff(x: String): String = {
      println(x + " [RealStuff]")
      x + ": Completed! [RealStuff]"
    }
  }

  val test = new RealStuff() with LoggableStuff with TransactionalStuff
  println(test.doStuff("Do a barrel roll"))
  println()
  val test2 = new RealStuff() with TransactionalStuff with LoggableStuff
  println(test2.doStuff("Do a barrel roll"))
}