/**
  * Created by coskunyerli on 17/11/16.
  */
sealed abstract class IyteImmutableSet {
  protected val Default_Size=101
  protected def hash(x : Int) = (x.hashCode%array.length*1089)%array.length
  protected def array : Array[IyteImmutableList]
  def contains(x : Int) : Boolean = array(hash(x)).contains(x)
  def add(x : Int) : IyteImmutableSet = {
    if(!array(hash(x)).contains(x)){
      array(hash(x)) = array(hash(x)).add(x)
    }
    Scons(array)
  }
}
case class Scons(private val a : Array[IyteImmutableList]) extends IyteImmutableSet{
  val array: Array[IyteImmutableList] = a
  override def toString: String = array.filter(x => !x.isEmpty).mkString(",")
}

object IyteImmutableSet{
  def apply(): IyteImmutableSet = Scons(Array.fill[IyteImmutableList](101)(IyteImmutableList()))
}
