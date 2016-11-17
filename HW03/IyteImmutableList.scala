sealed trait IyteImmutableList{
	def add(x : Int) :IyteImmutableList
}
case object Nill extends IyteImmutableList{
	override def add(x : Int) : IyteImmutableList = Cons(x,Nill)
	override def toString = ""
}
case class Cons(head: Int, tail: IyteImmutableList) extends IyteImmutableList{
	override def add(x : Int) : IyteImmutableList =  Cons(x,this)
	override def toString : String= {
		def go(l : IyteImmutableList) : String= l match {
			case Nill => Nill.toString
			case Cons(x,xs) => x.toString + "," + go(xs)
		}
		 val string=go(this)
		 string.substring(0,string.length-1)
	}
}
object IyteImmutableList{
	def apply() : IyteImmutableList = Nill
}