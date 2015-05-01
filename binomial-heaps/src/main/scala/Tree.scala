import leon._
import leon.lang._
import leon.annotation._
import leon.collection._

/*
 * Implementation of Tree based on "Purely Functionnal Data Structure, Okasaki, P71+"
 * @author Maëlle Colussi
 * @author Mathieu Demarne
 */

sealed abstract class Tree[T <: Ordered[T]] {
	def link(that: Tree[T]) : Tree[T] = {//t1: this, t2:that
		(this, that) match {
			case (TreeNode[T](r, x1, c1), TreeNode[T](_, x2, c2)) =>
				if (x1 <= x2) TreeNode[T](r + 1, x1, Cons(that, c1))
				else TreeNode[T](r + 1, x2, Cons(this, c2))
		}
	}
	
	def rank(): BigInt = this match {
		case TreeNode[T](r, x, c) => r
	}
	def root(): T = this match {
		case TreeNode[T](r, x, c) => x
	}
	
	//def size: BigInt = {???} ensuring (res => res == this.toList.size && res >= 0)
	//def toList: List[T] = {???} ensuring (res => this.content == res.content && res.size == this.size && res.size >= 0)
	//def content: Set[T] = {???} ensuring (res => res == this.toList.content /*&& res.size == this.toList.size*/)

}

case class TreeNode[T](r: BigInt, x: T, c: BinomialHeap[T]) extends Tree[T]

