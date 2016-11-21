/**
  * Created by coskunyerli on 17/11/16.
  */
sealed abstract class IyteImmutableSet {
  protected val Default_Size=101
  protected def hash(x : Int) = (x.hashCode%array.length*1089)%array.length
  protected def array : Array[IyteImmutableList]
  def contains(x : Int) : Boolean = array(hash(x)).contains(x)
  def add(x : Int) : IyteImmutableSet = {
    val temp : Array[IyteImmutableList]=new Array[IyteImmutableList](101)
    Array.copy(array,0,temp,0,101)
    if(!temp(hash(x)).contains(x)){
      temp(hash(x)) = temp(hash(x)).add(x)
    }
    Scons(temp)
  }
}
case class Scons(private val a : Array[IyteImmutableList]) extends IyteImmutableSet{
  val array: Array[IyteImmutableList] = a
  override def toString: String = array.filter(x => !x.isEmpty).mkString(",")
}

object IyteImmutableSet{
  def apply(): IyteImmutableSet = Scons(Array.fill[IyteImmutableList](101)(IyteImmutableList()))
}
