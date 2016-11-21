/**
  * Created by coskunyerli on 17/11/16.
  */
class IyteMutableSet {
  val Default_Size = 101
  private def hash(x : Int) : Int = (x.hashCode%array.length*1089)%array.length
  private val array = Array.fill[IyteMutableList](Default_Size)(IyteMutableList())
  def add(x : Int) : Unit =if(!array(hash(x)).contains(x)){
    array(hash(x)).add(x)
  }
  def contains(x : Int): Boolean = array(hash(x)).contains(x)
  override def toString(): String =  array.filter(x => !x.isEmpty).mkString(",")
}
object IyteMutableSet{
  def apply(): IyteMutableSet = new IyteMutableSet()
}
