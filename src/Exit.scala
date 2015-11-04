/*
 *@author Charles
 */
class Exit(val xitNum:Int) {
  // 1=N,2=S,3=E,4=W,5=U,6=D
  
}

object Exit {
  def apply(dest:String):Option[Exit] = {
    val n = dest.toInt
    if(n>=0) Some(new Exit(n))
    else None
  }
}