import leon._
import leon.lang._
import leon.annotation._
import leon.collection._

/*
 * Implementation of Tree based on "Purely Functionnal Data Structure, Okasaki, P71+"
 * @author Maëlle Colussi
 * @author Mathieu Demarne
 */


sealed abstract class Tree[T] {
	def link(that: Tree[T]) : Tree[T] = {
		
	}
}

case class TreeNode[T](r: BigInt, x: T, c: Tree[T]) extends Tree[T]
