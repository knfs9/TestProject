package aoptest

object AOPTest extends App {

  trait Stuff {
    def doStuff(): Unit
  }

  trait LoggableStuff extends Stuff {
    abstract override def doStuff(): Unit = {
      println("Logging Enter")
      super.doStuff()
      println("Logging Exit")
    }
  }

  trait TransactionStuff extends Stuff {
    abstract override def doStuff(): Unit = {
      println("TX Start")
      try {
        super.doStuff()
        println("TX Commit")
      } catch {
        case e: Exception => println("TX Rollback")
      }
    }
  }

  class RealStuff extends Stuff {
    def doStuff(): Unit = println("Doing some real stuff")
  }

  val stuff = new RealStuff
  stuff.doStuff()
  println

  val stuff2 = new RealStuff with LoggableStuff
  stuff2.doStuff()
  println

  val stuff3 = new RealStuff with LoggableStuff with TransactionStuff
  stuff3.doStuff()
  println

  val stuff4 = new RealStuff with TransactionStuff with LoggableStuff
  stuff4.doStuff()
  println

  trait RetryStuff extends Stuff {
    abstract override def doStuff(): Unit = {
      var times = 0
      var retry = true
      while (retry) {
        try {
          super.doStuff()
          retry = false
        } catch {
          case e: Exception =>
            if (times < 3) {
              times += 1
              println("Operation failed, retrying: " + times + " attempt")
            } else {
              retry = false
              throw e
            }
        }
      }
    }
  }

  class RealStuff2 extends Stuff {
    def doStuff(): Unit = {
      println("Doing real stuff")
      throw new Exception("Expected")
    }
  }

  val stuff5 = new RealStuff2 with RetryStuff with TransactionStuff with LoggableStuff
  stuff5.doStuff()

}