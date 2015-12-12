import java.io.PrintStream
import java.io.InputStream

class Player(val name: String, private var loc: Room, private var inv: List[Item], val ps: PrintStream, private val is: InputStream) {
  def room = loc

  val buf = collection.mutable.Buffer[Byte]()

  private def readLine: Option[String] = {
    if (is.available > 0) {
      val buf2 = new Array[Byte](is.available)
      is.read(buf2)
      buf ++= buf2
      if (buf(buf.length - 1) == '\n') {
        val ret = Some(new String(buf.toArray).trim)
        buf.clear()
        ret
      } else None
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
      println("< " + comm)
      ps.println("\nI'm sorry, Dave, I'm afraid I can't do that\n")
    }
  }

  val help = List("\nup for MOVE UP", "down for MOVE DOWN", "north for MOVE NORTH", "east for MOVE EAST", "\nwest for MOVE WEST", "get [itemname] for TAKE ITEM",
    "look for REPRINT ROOM DESCRIPTION", "\ninv for INSPECT INVENTORY", "drop [itemname] for DROP ITEM", "eat [itemanme] for EAT ITEM",
    "tip to TIP FEDORA\n", "tell [player name] [message]", "say [message]")

  def getFromInventory(itemName: String): Option[Item] = {
    inv.find(_.name == itemName) match {
      case Some(item) =>
        inv = inv.filter(_ != item)
        Some(item)

      case None =>
        ps.println("\nNot in inventory\n")
        None
    }
  }

  def move(dir: Int): Unit = {
    loc.getExit(dir) match {
      case Some(exit) =>
        loc.leaveRoom(this)
        loc = Room.mapRooms(exit.xitNum)
        loc.printDescription(this.ps)
        loc.enterRoom(this)

      case None => ps.println("\nNot a valid exit\n")
    }
  }

  def addToInventory(item: Item): Unit = {
    inv = item :: inv
  }

  def printInventory(): Unit = {
    if (inv != Nil) {
      ps.println(inv.map(_.name).mkString(", "))
    } else
      ps.println("\nNothing in Inventory\n")
  }

  def printRoom(): Unit = {
    loc.printDescription(this.ps)
  }

  def printHelp(): Unit = {
    ps.println(help.mkString(", "))
  }
 
  
  def findRoom(cur: Room, dest: Room, bread: List[Room]): List[Room] = {
    if (cur == dest) cur :: bread
    else {
      val paths = for (Some(e) <- cur.exitOut; if !bread.contains(Room.mapRooms(e.xitNum))) yield {
        findRoom(Room.mapRooms(e.xitNum),dest,cur::bread)
      }
      val outPaths = paths.sortBy { x => x.length }
      outPaths(0)
    }
  }
  def findRoomInterp(cur:Room,inp:String):Unit = {
    for(i<- Room.mapRooms){
      if(inp==i.name) ps.println(findRoom(cur,i,List[Room]()))
      else ps.println("\n\nnPlease check that you spelled the room name correctly!\n")
    }
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
          ps.println("\nPicked up " + args + "\n")
        case None =>
          ps.println("\nNot in room\n")
      }),
    "drop" -> ((args, p) =>
      p.getFromInventory(args) match {
        case Some(item) =>
          p.room.dropItem(item)
          ps.println("\nDropped " + item.name + "\n")
        case None =>
          ps.println("\nNot in inventory\n")
      }),

    "look" -> ((args, p) => p.printRoom),
    "inventory" -> ((args, p) => p.printInventory),
    "inv" -> ((args, p) => p.printInventory),
    "help" -> ((args, p) => p.printHelp),
    "eat" -> ((args, p) =>
      p.getFromInventory(args) match {
        case Some(item) =>
          ps.println("\nThat was delicious. Yes. Yummy.\n")
        case None =>
          ps.println("\nWhat are you eating? Is it air?...\n\n\nIs it good?\n")
      }),
    "tip" -> ((args, p) => ps.println("\nYou tip your fedora. So suave.\n")),
    "say" -> ((args, p) => p.loc.tellRoom(p.name + " said: " + args)),
    "tell" -> ((args, p) => p.loc.tellPlayer(p.name + " : " + args)),
    "shortPath" -> ((args, p) => p.findRoomInterp(p.loc,args)))
}