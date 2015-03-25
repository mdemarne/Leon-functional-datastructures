case class Queue[T](f : List[T], r: List[T]){
	def queueInv(f : List[T], r: List[T]): Queue[T] = {
		if (f.isEmpty) Queue(r.reverse, Nil)
		else Queue(f, r)
	}
	def head: T = { //put require non empty
		f match {
			case x :: xs => x 
			case Nil => sys.error("") //not possible if invariant holds
		}
	}
	
	def tail: Queue[T] = {
		f match {
			case x :: xs => Queue(xs, r)
			case Nil => sys.error("")
		}
	}
	
	def snoc(x: T): Queue[T] = Queue(f, x :: r)
	//TODO : change to leon's lists
	//TODO : invariant
}

