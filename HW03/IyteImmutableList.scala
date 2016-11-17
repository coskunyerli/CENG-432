sealed trait IyteImmutableList {
	def isEmpty:Boolean
	def add(x: Int): IyteImmutableList
	def contains(x: Int): Boolean = {
		def go(x: Int)(root: IyteImmutableList): Boolean = root match {
			case Cons(xv, xs) => if (xv == x) true else go(x)(xs)
			case Nill => false
		}
		go(x)(this)
	}
}
case object Nill extends IyteImmutableList{
	override def add(x : Int) : IyteImmutableList = Cons(x,Nill)
	override def toString = ""
	override def isEmpty :Boolean=true
}
case class Cons(head: Int, tail: IyteImmutableList) extends IyteImmutableList{
	override def add(x : Int) : IyteImmutableList =  Cons(x,this)
	override def toString : String= {
		def go(l : IyteImmutableList,string:String) : String= l match {
			case Nill => string.substring(0,string.length-1)
			case Cons(x,xs) => go(xs,string+x.toString + "," )
		}
		 go(this,"")
	}
	override 	def isEmpty : Boolean= false
}

object IyteImmutableList{
	def apply() : IyteImmutableList = Nill
}