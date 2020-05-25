package scarlet

///////////////////////////////////

package object util {

///////////////////////////////////

case class Percent(value: Double) extends Ordered[Percent] {
  require( value >= 0.0 && value <= 100.0, s"Required percentage, found $value" )
  
  def of(x: Double): Double = ( x * value ) / 100.0
  
  override def compare(that: Percent): Int = 
    java.lang.Double.compare(value,that.value)
    
  override def toString(): String = s"$value%"    
}

///////////////////////////////////

object Percent {
  implicit def percent2double(x: Percent): Double = x.value

  /////////////////////////////////
  
  import spire.algebra._
  import spire.implicits._
  import spire.math._
  import spire.syntax._
  
  implicit val asSpire = new Field[Percent] {
    // Members declared in algebra.ring.AdditiveMonoid 
    val zero: Percent = Percent(0.0)
    // Members declared in algebra.ring.MultiplicativeMonoid 
    val one: Percent = Percent(1.0)

    // Members declared in algebra.ring.AdditiveSemigroup 
    def plus(x: Percent,y: Percent): Percent = Percent(x.value + y.value) 
    // Members declared in algebra.ring.MultiplicativeSemigroup 
    def times(x: Percent,y: Percent): Percent = Percent(x.value * y.value)

    // Members declared in algebra.ring.MultiplicativeGroup 
    def div(x: Percent,y: Percent): Percent = Percent(x.value / y.value)     
    
    // Members declared in algebra.ring.AdditiveGroup 
    def negate(x: Percent): Percent = Percent(-x.value) 
    
    // Members declared in spire.algebra.GCDRing 
    def gcd(a: Percent,b: Percent)(implicit ev: Eq[Percent]): Percent = ??? 
    def lcm(a: Percent,b: Percent)(implicit ev: Eq[Percent]): Percent = ???
  }

  def main(args: Array[String]): Unit = {
      scarlet.math.ClosedOpenInterval(Percent(0),Percent(1))
  }
}

///////////////////////////////////

} // package object util {

///////////////////////////////////

// End ///////////////////////////////////////////////////////////////
