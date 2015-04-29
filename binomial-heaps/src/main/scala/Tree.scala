import leon._
import leon.lang._
import leon.annotation._
import leon.collection._

/*
 * Implementation of Tree based on "Purely Functionnal Data Structure, Okasaki, P71+"
 * @author Maëlle Colussi
 * @author Mathieu Demarne
 */

//TODO : how to write in Scala that we want such an object to exist for T ?
//--> pass as function something of type Ordering[T]
//--> or force as BigInt (and make a generic data structure copy afterwards)
object TOrdering[T] extends Ordering[T] {
  def compare(a:T, b:T) = ...
}

sealed abstract class Tree[T] {
	def link(that: Tree[T]) : Tree[T] = {
		(this, that) match {
			case (TreeNode[T](r, x1, c1), TreeNode[T](_, x2, c2)) =>
				if (TOrdering.lteq(x1, x2)) TreeNode[T](r + 1, x2, that :: c1)
				else TreeNode[T](r + 1, x2, this :: c2)
		}
	}
	
	def rank(): BigInt = this match {
		case TreeNode[T](r, x, c) => r
	}
	def root(): T = this match {
		case TreeNode[T](r, x, c) => x
	}
}

case class TreeNode[T](r: BigInt, x: T, c: Tree[T]) extends Tree[T]
