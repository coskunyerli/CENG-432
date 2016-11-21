class IyteMutableList(){
	case class Node(value:Int,var next : Node)
	private var first:Node = null
	private var last:Node = first

	def isEmpty : Boolean = first == null

	def contains(x : Int) : Boolean = {
		def go(x:Int)(root:Node): Boolean =if(root==null) false else{
			if(root.value==x) true
			else go(x)(root.next)
		}
		go(x)(first)
	}

	def add(x :Int):Unit={
		if(first == null){
			first=createNode(x)
			last=first
		}else{
			last.next=createNode(x)
			last=last.next
		}
	}
	override def toString() : String = {
		var temp = first
		var string = ""
		while(temp != null){
			string += temp.value + ","
			temp=temp.next
		}
		if(string.isEmpty) "" else string.substring(0,string.length-1)
	}
	private def createNode(x :Int) : Node = Node(x,null)
}

object IyteMutableList{
	def apply() : IyteMutableList = new IyteMutableList()
}