package scarlet.math

import spire.algebra._
import spire.implicits._
import spire.math._
import spire.syntax._

// import algebra._
// import algebra.instances._

import scala.math.Ordered

///////////////////////////////////

case class ClosedOpenInterval[T<%Ordered[T]](lower: T, upper: T)(
  implicit ev: AdditiveAbGroup[T]) 
  extends Ordered[ClosedOpenInterval[T]] {
  
  require( ClosedOpenInterval.isValid(lower, upper ), s"Required valid ClosedOpenInterval, found [$lower,$upper)" )

	///////////////////////////////

  def contains(x: T): Boolean = 
    lower <= x  && x < upper

  def overlaps(other: ClosedOpenInterval[T]): Boolean = 
    contains( other.lower ) || ( contains( other.upper ) && other.upper != lower )

  def contains(other: ClosedOpenInterval[T]): Boolean = 
    contains( other.lower ) && other.upper <= upper

  def isBefore(other: ClosedOpenInterval[T]): Boolean =
    upper < other.lower

  def isAfter(other: ClosedOpenInterval[T]): Boolean =
    lower >= other.upper
    
	///////////////////////////////
    
  val length = ev.minus(upper,lower)

  def map[U<%Ordered[U]:AdditiveAbGroup](f: T => U)(implicit ev: Ordered[U]): ClosedOpenInterval[U] = 
    ClosedOpenInterval(f(lower),f(upper))  

  def after(other: ClosedOpenInterval[T]): ClosedOpenInterval[T] = { 
    if(isAfter(other)) this else ClosedOpenInterval(other.upper, ev.plus(other.upper,length) )  
  } ensuring { _.isAfter(other) }
  
  def translateBy(x: T): ClosedOpenInterval[T] = 
    ClosedOpenInterval( ev.plus(lower,x),ev.plus(upper,x))
    
  /////////////////////////////////
  
  override def toString(): String = s"[ $lower, $upper)"

  override def compare(rhs: ClosedOpenInterval[T]): Int = 
    if( lower < rhs.lower ) -1 else { 
      if( lower > rhs.lower ) 1 else {
        if( upper < rhs.upper ) -1 else { 
          if( upper == rhs.upper ) 0 else 1 }}}
}

///////////////////////////////////

object ClosedOpenInterval {
  
  def isValid[T<%Ordered[T]](lower: T, upper: T): Boolean = 
    lower <= upper
  
  /////////////////////////////////
    
  def main(args: Array[String]): Unit = {
    ClosedOpenInterval[Int](0,1)
  }
}

// End ///////////////////////////////////////////////////////////////
