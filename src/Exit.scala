/*
 *@author Charles
 */
class Exit(val xitNum:Int)
object Exit {
  def apply(dest:String):Option[Exit] = {
    val n = dest.toInt
    if(n>=0) Some(new Exit(n))
    else None
  }
}