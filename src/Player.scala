import java.io.PrintStream
import java.io.InputStream

class Player(val name: String, private var loc: Room, private var inv: List[Item], val ps: PrintStream, private val is: InputStream) {
  val buf = collection.mutable.Buffer[Byte]()
  
  //Room.mapRooms(Room.mapRooms(loc.getExit(loc))).enterRoom(this) // create a player. It enters a room.

  // non-blocking readLine
  private def readLine: Option[String] = {
    if (is.available > 0) {
      val buf2 = new Array[Byte](is.available)
      is.read(buf2)
      buf ++= buf2
      if(buf(buf.length-1) == '\n') {
        val ret = Some(new String(buf.toArray).trim)
        buf.clear()
        ret
      }
      
      else None
    } else None
  }

  def update(): Unit = {
    readLine.foreach(s => takeCommand(s.trim, this))
  }

  def takeCommand(input: String, player: Player): Unit = {
    println("I hear ya, loud and clear")
    val (comm, args) = {
      val i = input.indexOf(" ")
      if (i < 0) (input, "") else input.splitAt(i)
    }
    if (commands.contains(comm)) commands(comm)(args.trim, player)
    else {
      println("<" + comm)
      ps.println("I'm sorry, Dave, I'm afraid I can't do that")
    }
  }

  val help = List("u for MOVE UP", "d for MOVE DOWN", "n for MOVE NORTH", "e for MOVE EAST", "\nw for MOVE WEST", "get [itemname] for TAKE ITEM",
    "look for REPRINT ROOM DESCRIPTION", "\ninv for INSPECT INVENTORY", "drop [itemname] for DROP ITEM", "eat [itemanme] for EAT ITEM",
    "tip to TIP FEDORA")

  def getFromInventory(itemName: String): Option[Item] = {
    inv.find(_.name == itemName) match {
      case Some(item) =>
        inv = inv.filter(_ != item)
        Some(item)

      case None =>
        ps.println("Not in inventory")
        None
    }
  }
  def room = loc
  def move(dir: Int): Unit = {
    loc.getExit(dir) match {
      case Some(exit) =>
        Room.mapRooms(exit.xitNum).leaveRoom(this)
        loc = Room.mapRooms(exit.xitNum)
        loc.printDescription(ps)

      case None => ps.println("Not a valid exit")
    }
  }

  def addToInventory(item: Item): Unit = {
    inv = item :: inv
  }

  def printInventory(): Unit = {
    if (inv != Nil) {
      ps.println(inv.map(_.name).mkString(", "))
    } else
      ps.println("Nothing in Inventory")
  }

  def printRoom(): Unit = {
    loc.printDescription(ps)
  }

  def printHelp(): Unit = {
    ps.println(help.mkString(", "))
  }

  val commands = Map[String, (String, Player) => Unit](
    "north" -> ((args, p) => p.move(0)),
    "n" -> ((args, p) => p.move(0)),
    "south" -> ((args, p) => p.move(1)),
    "s" -> ((args, p) => p.move(1)),
    "east" -> ((args, p) => p.move(2)),
    "e" -> ((args, p) => p.move(2)),
    "west" -> ((args, p) => p.move(3)),
    "w" -> ((args, p) => p.move(3)),
    "up" -> ((args, p) => p.move(4)),
    "u" -> ((args, p) => p.move(4)),
    "down" -> ((args, p) => p.move(5)),
    "d" -> ((args, p) => p.move(5)),
    "get" -> ((args, p) =>
      p.room.getItem(args) match {
        case Some(item) =>
          p.addToInventory(item)
          ps.println("Picked up " + args)
        case None =>
          ps.println("Not in room")
      }),
    "drop" -> ((args, p) =>
      p.getFromInventory(args) match {
        case Some(item) =>
          p.room.dropItem(item)
          ps.println("Dropped " + item.name)
        case None =>
          ps.println("Not in inventory")
      }),

    "look" -> ((args, p) => p.printRoom),
    "inventory" -> ((args, p) => p.printInventory),
    "inv" -> ((args, p) => p.printInventory),
    "help" -> ((args, p) => p.printHelp),
    "eat" -> ((args, p) =>
      p.getFromInventory(args) match {
        case Some(item) =>
          ps.println("That was delicious. Yes. Yummy.")
        case None =>
          ps.println("What are you eating? Is it air?...\n\n\nIs it good?")
      }),
    "tip" -> ((args, p) => ps.println("You tip your fedora. So suave.")) //,
    // "say"  -> ((args, p) => Room.mapRooms(loc.getExit()).tellRoom(p.name + " said: " + args))
    )
}