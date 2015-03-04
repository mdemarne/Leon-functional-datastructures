import leon.lang._
 
/**
 * 1) Implement operations on rationals: addition, substraction, multiplication and division
 *    Ensure that the results are rationals, add necessary preconditions.
 * 2) Implement rational equivalence (~).
 * 3) Write lemmas that show that if a1 ~ a2, and b1 ~ b2, then (a1 <op> b1) ~ (a2 <op> b2), for each <op>
 */
object Rationals {
  // Represents n/d
  case class Q(n: BigInt, d: BigInt) {
 
    def +(that: Q):Q = {
      require(this.isRational && that.isRational)
      Q(this.n * that.d + that.n * this.d, this.d * that.d)
    } ensuring (_.isRational)
 
    def -(that: Q):Q =  {
      require(this.isRational && that.isRational)
      Q(this.n * that.d - that.n * this.d, this.d * that.d)
    } ensuring (_.isRational)
 
    def *(that: Q):Q =   {
      require(this.isRational && that.isRational)
      Q(this.n * that.n, this.d * that.d)
    } ensuring (_.isRational)
 
    def /(that: Q):Q = {
      require(this.isRational && that.isRational && that.nonZero)
      Q(this.n * that.d, this.d * that.n)
    } ensuring (_.isRational)
 
    // Equivalence of two rationals, true if they represent the same real number
    def ~(that: Q): Boolean = {
      require(this.isRational && that.isRational)
      that.n * this.d == that.d * this.n
    }
 
    def isRational = !(d == 0)
    def nonZero    = !(n == 0)
  }
 
  def lemma1(a1: Q, a2: Q, b1: Q, b2: Q): Boolean = {
    require {
      a1.isRational &&
      a2.isRational &&
      b1.isRational &&
      b2.isRational &&
      a1 ~ a2 && b1 ~ b2
    }
    val eq1 = (a1 + b1) ~ (a2 + b2)
    val eq2 = (a1 - b1) ~ (a2 - b2)
    val eq3 = (a1 * b1) ~ (a2 * b2)
    val eq4 = !a2.nonZero || !b2.nonZero || ((a1 / b1) ~ (a2 / b2))
    eq1 && eq2 && eq3 && eq4
  }.holds  
}
