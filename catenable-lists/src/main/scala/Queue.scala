/* 
 * Implementation of Queue based on "Purely Functionnal Data Structure, Okasaki, P15+" 
 * @author Maëlle Colussi
 * @author Mathieu Demarne
 */

object Queue {

	val empty = Queue(Nil, Nil)

}

case class Queue[T](f : List[T], r: List[T]) {

	def isEmpty = f.isEmpty && r.isEmpty

	/* TODO: use for invariant checking or remove ? */
	def queueInv(f : List[T], r: List[T]): Queue[T] = f match {
		case Nil => Queue(r.reverse, Nil)
		case _ => Queue(f, r)
	}

	def head: T = f.head //put require non empty
	
	def tail: Queue[T] = { // put require non empty
		f.tail match {
			case Nil => Queue(r.reverse, Nil)
			case xs => Queue(xs, r)
		}
	}
	
	def snoc(x: T): Queue[T] = f match {
		case Nil => Queue(x :: Nil, Nil)
		case _ =>  Queue(f, x :: r)
	}

	//TODO : change to leon's lists
	//TODO : invariant
}

