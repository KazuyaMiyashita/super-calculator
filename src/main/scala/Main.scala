import scala.io.StdIn.readLine

import supercalc.Eval

object Main extends App {

  def isExit(s: String): Boolean = s == null || s == "exit"
  val eval = new Eval

  var continue = true
  while(continue) {
    print("> ")
    readLine match {
      case x if isExit(x) => println("Bye!"); continue = false
      case x => println(eval(x))
    }
  }

}
