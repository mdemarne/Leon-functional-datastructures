import leon._
import leon.lang._
import leon.annotation._
import leon.collection._

/*
 * Implementation of Tree based on "Purely Functionnal Data Structure, Okasaki, P71+"
 * @author Maëlle Colussi
 * @author Mathieu Demarne
 */

sealed abstract class TreeBI {
	def link(that: TreeBI) : TreeBI = {//t1: this, t2:that
		(this, that) match {
			case (TreeNode(r, x1, c1), TreeNode(_, x2, c2)) =>
				if (x1 <= x2) TreeNode(r + 1, x1, BHList(Cons(that, 
					c1 match {case BHList(f) => f})))
				else TreeNode(r + 1, x2, BHList(Cons(this, 
					c2 match {case BHList(f) => f})))
		}
	} ensuring (res => res.size == this.size + that.size) //TODO : more ?

	def rank: BigInt = this match {
		case TreeNode(r, x, c) => r
	}

	def root: BigInt = this match {
		case TreeNode(r, x, c) => x
	}

	def size: BigInt = {
		this match {
			case TreeNode(r, x, c) => 1 + c.size
		}
	} ensuring (_ >= 0)

	def toList: List[BigInt] = {
		this match {
			case TreeNode(r, x, c) => Cons(r, c.toList)
		}
	} ensuring (res => res.size == this.size)

	def content: Set[BigInt] = {
		this match {
			case TreeNode(r, x, c) => Set(x) ++ c.content
		}
	}

}

case class TreeNode(r: BigInt, x: BigInt, c: BinomialHeapBI) extends TreeBI

