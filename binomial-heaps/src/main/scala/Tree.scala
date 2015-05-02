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
			case (TreeNode(r, x1, c1), TreeNode(_, x2, c2)) =>
				if (x1.<= x2) TreeNode(r + 1, x1, BHList(Cons(that, c1.f /*match {
					case BHList(f) => f
					case BHEmpty() => Nil[Tree[T]]()}*/)))
				else TreeNode(r + 1, x2, BHList(Cons(this, c2.f /*match {
					case BHList(f) => f
					case BHEmpty() => Nil[Tree[T]]()}*/)))
		}
	}
	
	def rank(): BigInt = this match {
		case TreeNode(r, x, c) => r
	}
	def root(): T = this match {
		case TreeNode(r, x, c) => x
	}
	
	//def size: BigInt = {???} ensuring (res => res == this.toList.size && res >= 0)
	//def toList: List[T] = {???} ensuring (res => this.content == res.content && res.size == this.size && res.size >= 0)
	//def content: Set[T] = {???} ensuring (res => res == this.toList.content /*&& res.size == this.toList.size*/)

}

case class TreeNode[T <: Ordered[T]](r: BigInt, x: T, c: BinomialHeap[T]) extends Tree[T]

