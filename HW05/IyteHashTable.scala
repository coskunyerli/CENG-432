/**
  * Created by coskunyerli on 10/12/16.
  */
class IyteHashTable(cap : Int) {

  case class Node(key: String, hash: Int, var value: String, var next: Node) {

  }
  def this() = this(16)
  private val default_size = 16
  private var initial_size = if(cap <= 0) default_size else cap
  private val load_factor = 0.75
  var array: Array[Node] = new Array[Node](initial_size)
  private var size = 0
  var threshold = load_factor * initial_size


  def index(hcode: Int) = {
    (hcode >>> 1) % initial_size
  }


  def newHashCode(s : String): Int = {
    var hash : Int = 13
    for (c <- s) {
      hash = ((hash << 5) + hash) + c
    }
    hash
  }

  private def findNode(index: Int, key: String): Node = {
    var temp = array(index)
    while (temp != null && temp.key != key) {
      temp = temp.next
    }
    temp
  }

  def resize(newSize : Int): Unit = {
    initial_size = newSize
    threshold = load_factor * initial_size
    val old_array = array
    array = new Array[Node](initial_size)
    var i = old_array.length - 1
    while (i >= 0) {
      var n: Node = old_array(i)
      while (n != null) {
        val index = this.index(n.hash)
        val n1 = n.next
        n.next = array(index)
        array(index) = n
        n = n1
      }
      i = i - 1
    }
  }

  def set(key: String, value: String): Unit = {
    val hashcode = newHashCode(key)
    val index: Int = this.index(hashcode)
    val n = findNode(index, key)
    if (n == null) {
      val newNode = Node(key, hashcode, value, array(index))
      array(index) = newNode
      size += 1
      if (size > threshold ) {
        resize(initial_size << 1)
      }
    }else{
      n.value = value
    }
  }

  def get(key: String): String = {
    val index: Int =this.index(newHashCode(key))
    val n = findNode(index, key)
    if (n ne null) n.value else null
  }

}

object IyteHashTable{
  def apply(): IyteHashTable = new IyteHashTable()
}
