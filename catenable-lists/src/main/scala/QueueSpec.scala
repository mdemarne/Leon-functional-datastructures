import leon._
import leon.lang._
import leon.annotation._
import leon.collection._

/*
 * Implementation of Queue based on "Purely Functionnal Data Structure, Okasaki, P15+"
 * @author Maëlle Colussi
 * @author Mathieu Demarne
 */

 // TODO: pass --xlang as option to Leon for inline expansion
object QueueSpec {

  def test1 {
      //TODO: do better testing!
      val Q1 = QEmpty[Int]()
      val Q2 = Q1.snoc(4)
      if (Q2.size != 1) error[Unit]("Wrong size!")
  }

}
