import scala.xml.Node

/*
 *
 * @author Charles
 */
class Item(val name:String,desc:String)

object Item{
  def apply(node:Node):Item = {
    val name = (node \ "@name").text
    val desc = (node.text)
    new Item(name,desc)
  }
  def printItemDescript():Unit = (
    println(Item)
  )
}