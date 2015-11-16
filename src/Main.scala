import scala.io.StdIn._
import java.net.ServerSocket
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import java.io.PrintStream

object Main {

  // to entesr game, open terminal, and say "telent localhost 6464"

  private var players: List[Player] = Nil

  def main(args: Array[String]): Unit = {
    val ss = new ServerSocket(4664)
    Future {
      while (true) {
        Thread.sleep(100)
        println("Accepting...")
        val soc = ss.accept()
        val ps = new PrintStream(soc.getOutputStream())
        val is = soc.getInputStream
        ps.println("What is your name? TELL ME YOUR NAME.")
        Future {
          while (is.available < 1) Thread.sleep(100)
          val buf = new Array[Byte](is.available) 
          is.read(buf)
          val name = new String(buf).trim
          players ::= new Player(name, Room.mapRooms(0), Nil, ps, is)
          println("Got Connection")
          players.head.ps.println("Welcome to a wonderful adventure full of conspiracies and conspiracy theories!\nYour adventure begins with you descending from this dream. \nType 'help' to see a list of commands\n")
        }
      }
    }
    while (true) {
      for (p <- players.indices) {
        Thread.sleep(100)
        players(p).update
      }
    }
  }
}