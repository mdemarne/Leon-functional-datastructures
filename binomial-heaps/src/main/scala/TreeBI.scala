import leon._
import leon.lang._
import leon.annotation._
import leon.collection._

/*
 * Implementation of Tree based on "Purely Functionnal Data Structure, Okasaki, P71+"
 * @author Maëlle Colussi
 * @author Mathieu Demarne
 */

sealed abstract class Tree {

	/* Lower-lever API */

	def link(that: Tree) : Tree = {
		require (this.hasProperShape && that.hasProperShape && this.r == that.r)
		val res: Tree = (this, that) match {
			case (t1, t2) if t1.x <= t2.x => TreeNode(t1.r + 1, t1.x, t2 :: t1.c)
			case (t1, t2) => TreeNode(t2.r + 1, t2.x, t1 :: t2.c)
		}
		res
	} ensuring (res => res.size == this.size + that.size && res.hasProperShape && res.content == this.content ++ that.content)

	def size: BigInt = {
		require(this.hasProperShape)
		val res: BigInt = 1 + Tree.treeListToCount(this.c)
		res
	} ensuring (_ >= 0)

	def toList: List[BigInt] = {
		require(this.hasProperShape)
		val res: List[BigInt] = Cons(this.x, Tree.treeListToList(this.c))
		res
	} ensuring (res => res.size == this.size)

	def content: Set[BigInt] = {
		require(this.hasProperShape)
		val res: Set[BigInt] = Set(this.x) ++ Tree.treeListToContent(this.c)
		res
	}
	
	/* Invariants */
	
	def hasProperShape: Boolean = {
		this.hasMinHeapProp && 
		Tree.treeListHasDecrRanks(this :: this.c) &&
		this.c.forall(_.hasProperShape)
	}

	/* Each binomial tree in a heap obeys the minimum-heap property: The key of a node is greater than or equal to the key of its parent. */
	private def hasMinHeapProp: Boolean = this.c.forall(_.x >= this.x)

	/* Helpers */

	implicit def treeToNode(t: Tree): TreeNode = t match { case n: TreeNode => n }

}

/* Companion object */
object Tree {

	/* Lower-level API */

	def apply(v: BigInt): Tree = TreeNode(0, v, Nil())

	/* Invariants */

	def treeListHasDecrRanks(c: List[Tree]): Boolean = c match {
			case Nil() => true
			case Cons(t, Nil()) => true
			case Cons(t1, ts @ Cons(t2, _)) => t2.r == t1.r - 1 && Tree.treeListHasDecrRanks(ts)
	}

	/* Helpers */

	/* Summing size of trees in a list */
	def treeListToCount(l: List[Tree]): BigInt = {
		require(l.forall(_.hasProperShape))
		val res: BigInt = l match {
			case Nil() => 0
			case Cons(t, ts) => t.size + treeListToCount(ts)
		}
		res
	} ensuring (_ >= 0)

	def treeListToList(l: List[Tree]): List[BigInt] = {
		require(l.forall(_.hasProperShape))
		val res: List[BigInt] = l match {
			case Nil() => Nil()
			case Cons(t, ts) => t.toList ++ treeListToList(ts)
		}
		res
	}

	def treeListToContent(l: List[Tree]): Set[BigInt] = {
		require(l.forall(_.hasProperShape))
		val res: Set[BigInt] = l match {
			case Nil() => Set()
			case Cons(t, ts) => t.content ++ treeListToContent(ts)
		}
		res
	}

	implicit def treeToNode(t: Tree): TreeNode = t match { case n: TreeNode => n }

}

case class TreeNode(r: BigInt, x: BigInt, c: List[Tree]) extends Tree