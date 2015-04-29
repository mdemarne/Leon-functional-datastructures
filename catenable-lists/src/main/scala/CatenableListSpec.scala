import leon._
import leon.lang._
import leon.annotation._
import leon.collection._
import leon.collection.ListOps._

/*
 * Implementation of Catenable List based on "Purely Functionnal Data Structure, Okasaki, P93+" - test file
 * @author Maëlle Colussi
 * @author Mathieu Demarne
 */

 // TODO: pass --xlang as option to Leon for inline expansion
object CatenableListSpec {

  def testSnocCons {
      val l0 = CEmpty[BigInt]()
      val l1 = l0.snoc(4).snoc(5)
      val l2 = l0.snoc(5).cons(4)
      if (l1 != l2) error[Unit]("Wrong elements!")
  }

  def testMergeAndHead {
  	  val l1 = CEmpty[BigInt]().cons(1).cons(2).cons(3)
  	  val l2 = CEmpty[BigInt]().cons(4).cons(5).cons(6)
  	  val lt1 = l1 ++ l2
  	  if (!l1.forall(lt1.contains(_)) || !l2.forall(lt1.contains(_))) error[Unit]("Wrong testMerge")
  	  
  	  val l3 = CEmpty[BigInt]().cons(7).cons(8).cons(9)
  	  val lt2 = lt1 ++ l3
  	  if (!l1.forall(lt2.contains(_)) || !l2.forall(lt2.contains(_)) && !l3.forall(lt2.contains(_))) error[Unit]("Wrong testMerge")
  	  
  	  val lt3 = lt1.snoc(0)
  	  if (lt3.head != 0) error[Unit]("Wrong testMerge")
  	  if (lt3.size != 10) error[Unit]("Wrong testMerge")
  }

  def testHeadAndTail {
  	val l1 = CEmpty[BigInt]().cons(3).cons(4).snoc(2).cons(5).cons(6).snoc(1)
  	val l2 = l1 ++ CEmpty[BigInt]().cons(8).snoc(7)
  	val l3 = CEmpty[BigInt]().cons(0).snoc(-1) ++ l2

  	if (l3.size != 10) error[Unit]("Wrong testHeadAndTail")
  	if (l3.head != -1) error[Unit]("Wrong testHeadAndTail")
  	if (l3.tail.head != 0) error[Unit]("Wrong testHeadAndTail")
  	if (l3.tail.tail.head != 1) error[Unit]("Wrong testHeadAndTail")
  	if (l3.tail.tail.tail.tail.tail.tail.head != 5) error[Unit]("Wrong testHeadAndTail")
  }

  def testTail {
  	val l1 = CEmpty[BigInt]().cons(3)
  	if (l1.tail.isDefined) error[Unit]("Wrong testTail")
  }
}
