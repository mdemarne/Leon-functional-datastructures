import leon.lang._
import leon.collection._
import leon._
/**
 * 1) Implement the isSearchTree property that checks bounds of elements in a
 *    search tree. Assume that the tree is strictly sorted (no dupplicates)
 *
 * 2) Implement operations on Binary Search Trees as efficiently as you can.
 *    These operations will likely not verify, but make sure that leon does not
 *    find counter-examples within a reasonnable timeout (e.g. --timeout=5 )
 *
 *    You do not need to change the pre-/post-conditions
 *
 * 3) Implement toList to return a sorted list from a search tree.
 */

object BinaryTree {

  abstract class Tree {
    def content: Set[BigInt] = {
      this match {
        case Empty => Set()
        case Node(l, v, r) => l.content ++ Set(v) ++ r.content
      }
    }

    def size: BigInt = {
      this match {
        case Empty => 0
        case Node(l, _, r) => l.size + r.size + 1
      }
    } ensuring { _ >= 0 }


    def +(x: BigInt): Tree = {
      require(isBT)
      this match { // DONE
        case Node(l, v, r) if x < v => Node(l + x, v, r)
        case Node(l, v, r) if v < x => Node(l, v, r + x)
        case n: Node => n
        case Empty => Node(Empty, x, Empty)
      }
    } ensuring {
      res => res.isBT &&
        res.content == this.content ++ Set(x) &&
        res.size >= this.size &&
        res.size <= this.size + 1
    }

    def -(x: BigInt): Tree = {
      require(isBT)
      this match { // DONE

        /* Recursion in the tree */
        case Empty => Empty /* Nothing to remove, x is not in the tree */
        case Node(l, v, r) if x < v => Node(l - x, v, r)
        case Node(l, v, r) if v < x => Node(l, v, r - x)

        /* Actively removing nodes */
        case Node(Empty, v, Empty) => Empty
        case Node(Empty, v, r) => r
        case Node(l, v, Empty) => l
        case Node(l, v, r) => l ++ r
      }
    } ensuring {
      res => res.isBT &&
        res.content == this.content -- Set(x) &&
        res.size <= this.size &&
        res.size >= this.size - 1
    }

    def ++(that: Tree): Tree = {
      require(this.isBT && that.isBT)
      this match {
        case Empty => that
        case Node(l, v, r) =>
          that match {
            case Empty => this
            case Node(lThat, vThat, rThat) if vThat == v => Node(l ++ lThat, v, r ++ rThat)
            case Node(lThat, vThat, rThat) if vThat < v => rThat ++ Node((l + vThat) ++ lThat, v, r)
            case Node(lThat, vThat, rThat) if v < vThat => Node(l, v, (r + vThat) ++ rThat) ++ lThat
          }
      }
      /* The most beautiful version, saved for the records */
      /* that match {
        case Empty => that
        case Node(l, v, r) => (this + v) ++ l ++ r
      } */
    } ensuring {
      res => res.isBT && res.content == this.content ++ that.content
    }

    def contains(x: BigInt): Boolean = {
      require(isBT)
      this match {
        case Empty => false
        case Node(l, v, r) if x == v => true
        case Node(l, v, r) if x < v => l contains x
        case Node(l, v, r) if v < x => r contains x
      }
    } ensuring {
      res => res == (content contains x)
    }

    def toList: List[BigInt] = {
      require(isBT)
      this match {
        case Empty => Nil[BigInt]
        case Node(l, v, r) => l.toList ++ (v :: r.toList)
      }
    } ensuring {
      res => res.content == this.content && isSorted(res)
    }

    // Properties

    def isBT: Boolean = {
      isSearchTree(None(), None())
    }

    def isSearchTree(min: Option[BigInt], max: Option[BigInt]): Boolean = {
      this match {
        case Empty => true
        case Node(l, v, r) =>
          val isValidNode = (min, max) match {
            case (Some(mn), Some(mx)) => mn < v && v < mx
            case (Some(mn), _ ) => mn < v
            case (_, Some(mx)) => v < mx
            case _ => true
          }
          if (!isValidNode) false
          else l.isSearchTree(min, Some(v)) && r.isSearchTree(Some(v), max) // DONE
      }
    }
  }

  case object Empty extends Tree
  case class Node(l: Tree, v: BigInt, r: Tree) extends Tree


  def isSorted(l: List[BigInt]): Boolean = {
    l match {
      case Cons(v1, t @ Cons(v2, _)) if v1 >= v2 => false
      case Cons(h, t) => isSorted(t)
      case Nil() => true
    }
  }
}


