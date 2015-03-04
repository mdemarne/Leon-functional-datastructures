//first part ok perhaps
//+ and contains ok perhaps
//-, toList : ask question
//++ : also ask question

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
      this match {
        case Empty => Node(Empty, x, Empty)
        case Node(l, v, r) =>
          if (v == x) this
          else if (x < v) Node(l + x, v, r)
          else Node(l, v, r + x)
      } // TODO
    } ensuring {
      res => res.isBT &&
             res.content == this.content ++ Set(x) &&
             res.size >= this.size &&
             res.size <= this.size + 1
    }
 
    def -(x: BigInt): Tree = {
      require(isBT)
      this match {
        case Empty => this
        case Node(l, v, r) =>
        if (x == v) {
          if (l.size == 0) r 
          else if (r.size == 0) l
          else {
            var y1 = l.rightElt().get
            Node(l - y1, y1, r)
          }
        }
        else if (x < v) Node(l - x, v, r)
        else Node(l, v, r - x)
      }// TODO
    } ensuring {
      res => res.isBT &&
             res.content == this.content -- Set(x) &&
             res.size <= this.size &&
             res.size >= this.size - 1
    }
    
    // 
    def rightElt() : Option[BigInt] = {
      require(isBT)
      this match {
        case Empty => None[BigInt]
        case Node(l, v, r) => 
        if(r.size == 0) Some(v)
        else r.rightElt()
      }
    } ensuring { res => (!res.isDefined || this.contains(res.get)) && (this.size == 0 || res.isDefined)}
    
 
    def ++(that: Tree): Tree = {
      require(this.isBT && that.isBT)
      
      def listToTree(l: List[BigInt], buf : Tree) : Tree = {
        require(isSorted(l.reverse))
        l match {
          case Nil() => buf
          case Cons(h, t) => listToTree(t, Node(Empty, h, buf))
        }
      } ensuring{_.isBT}
      
      def mergeLists(l1: List[BigInt], l2: List[BigInt], buf: List[BigInt]) : List[BigInt] = {
        require(isSorted(l1) && isSorted(l2))
        if(l1.isEmpty) l2.reverse ++ buf
        else if (l2.isEmpty) l1.reverse ++ buf
        else if (l1.head < l2.head){
          if (!buf.isEmpty && l1.head == buf.head) mergeLists(l1.tail, l2, buf)
          else mergeLists(l1.tail, l2, List(l1.head) ++ buf)
        }
        else if (l1.head == l2.head) mergeLists(l1.tail, l2, buf)
        else {
          if (!buf.isEmpty && l2.head == buf.head) mergeLists(l1, l2.tail, buf)
          else mergeLists(l1, l2.tail, List(l2.head) ++ buf)
        }
      } ensuring{res => res.size <= l1.size + l2.size && res.content == l1.content ++ l2.content && isSorted(res.reverse)}
      
      listToTree(mergeLists(this.toList, that.toList, Nil()), Empty)// TODO
    } ensuring {
      res => res.isBT && res.content == this.content ++ that.content
    }
    
 
    def contains(x: BigInt): Boolean = {
      require(isBT)
      this match {
        case Empty => false
        case Node(l, v, r) => 
          (v == x) || (x < v && l.contains(x)) || (x > v && r.contains(x))
      } // TODO
    } ensuring {
      res => res == (content contains x)
    }
 
    def toList: List[BigInt] = {
      require(isBT)
      this match {
        case Empty => Nil()
        case Node(l, v, r) => l.toList ++ List(v) ++ r.toList
      } // TODO
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
          (!min.isDefined || min.get < v) &&
          (!max.isDefined || max.get > v) &&
          l.isSearchTree(min, Some(v)) && r.isSearchTree(Some(v), max)// TODO
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