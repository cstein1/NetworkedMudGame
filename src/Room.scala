import scala.xml._
import java.io.PrintStream

class Room(val name: String, desc: String, private var items: List[Item], exits: Array[Option[Exit]], private var players: List[Player]) {
  val ExitNames = List("North", "South", "East", "West", "Up", "Down")

  def exitOut = exits
  
  def enterRoom(p: Player): Unit = {
    for (p2 <- players) {
      if (p2.room == p.room && p2 != p) p2.ps.println("\n" + p.name + " walked in.\n")
    }
    players ::= p
  }

  def leaveRoom(p: Player): Unit = {
    players = players.filter(_ != p)
    for (p2 <- players) {
      if (p2.room == p.room && p2 != p) p2.ps.println("\n" + p.name + " has left the room.\n")
    }
  }

  def printDescription(ps: PrintStream): Unit = {
    if (name != "Outside") {
      ps.println("\nYou are in " + name + ".\n" + desc + "\n")
    } else { ps.println("\nYou are " + name + ".\n" + desc + "\n") }
    if (items == Nil) { ps.println("") }
    else {
      ps.println("ITEMS: ")
      ps.println(items.map(_.name).mkString(", "))
    }
    ps.println("EXITS: ")
    for ((Some(e), dir) <- exits.zip(ExitNames)) {

      ps.println(Room.mapRooms(e.xitNum).name + " is " + dir)
    }
    ps.println("PLAYERS: ")

    for (p <- players) {
      ps.println(p.name)
    }
  }
  def tellRoom(s: String): Unit = {
    for (p2 <- players.indices) players(p2).ps.println(s)
  }

  def tellPlayer(s: String) {
    var p = s.split(":")(1).split("\\s+")(1).trim
    println(p)
    var args = s.split(": ")(1).split("\\s+").dropWhile { x => x==p }
    println(args.mkString)
    var pOfI = players(0)
    for (i <- players.indices) {
      if (p == players(i).name) {
        pOfI = players(i) //player of interest
        pOfI.ps.println(p.mkString(" ") + " says: " + args.mkString(" "))
      }
    }
  }

  def getExit(dir: Int): Option[Exit] = {
    exits(dir)
  }

  def getItem(itemName: String): Option[Item] = {
    items.find(_.name == itemName) match {
      case Some(item) =>
        items = items.filter(_ != item)
        Some(item)

      case None => None
    }
  }

  def dropItem(item: Item): Unit = {
    items ::= item
  }
  
  def thisRoomNum:Int = {
    var rover = 0
    while (true) {
      if(Room.mapRooms(rover) == this) return rover
      else rover += 1
    }
    -1
  }
}

object Room {
  val mapRooms = readMap

  
  def readMap: Array[Room] = {
    val xml = XML.loadFile("map.xml")
    (xml \ "room").map(n => Room(n)).toArray
  }

  def apply(node: Node): Room = {
    val name = (node \ "@name").text // @ means it is an attribute. (the <attribute> in the xml file)
    val desc = (node \ "description").text // @ is not necessary for sub elements
    val items = (node \ "item").map(n => Item(n)).toList
    val exits = (node \ "exit").text.split(",").map(s => Exit(s))
    new Room(name, desc, items, exits, Nil)
  }
}