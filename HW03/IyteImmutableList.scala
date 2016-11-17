sealed trait IyteImmutableList{
	def add(x : Int) :ImmutableList
}
case object Nill extends IyteImmutableList{
	override def add(x : Int) : ImmutableList = Cons(x,Nill)
	override def toString = ""
}
case class Cons(head: Int, tail: ImmutableList) extends IyteImmutableList{
	override def add(x : Int) : ImmutableList =  Cons(x,this)
	override def toString : String= {
		def go(l : ImmutableList) : String= l match {
			case Nill => Nill.toString
			case Cons(x,xs) => x.toString + "," + go(xs)
		}
		 val string=go(this)
		 string.substring(0,string.length-1)
	}
}
object IyteImmutableList{
	def apply() : ImmutableList = Nill
}