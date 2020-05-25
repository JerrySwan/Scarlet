package scarlet.util

import scala.math.Ordered

import spire.algebra._
import spire.implicits._
import scala.math.Ordering.Implicits._

// import algebra._
// import algebra.ring._
// import algebra.instances.all._

// import algebra._
// import algebra.ring._
// import algebra.instances.all._

///////////////////////////////////

final case class ClosedInterval[T<%Ordered[T]](lower: T, upper: T) {
  require( lower <= upper )
  
  def contains(x: T): Boolean = 
    lower <= x && x <= upper

  def length(implicit ev: AdditiveGroup[T]): T = 
    ev.minus(upper,lower)
    
  def translateBy(x: T)(implicit ev: AdditiveAbGroup[T]): ClosedInterval[T] = 
    ClosedInterval( ev.plus(lower,x),ev.plus(upper,x))
}

///////////////////////////////////

object ClosedInterval {
  
  def point[T<%Ordered[T]](value: T): ClosedInterval[T] = 
    ClosedInterval(value,value) 
  
  def mkSafe[T<%Ordered[T]](a: T, b: T): ClosedInterval[T] = {
    val ord = implicitly[Ordering[T]]
    ClosedInterval(ord.min(a,b), ord.max(a,b))
  }

  /////////////////////////////////
  
  def selectUniform(interval: ClosedInterval[Int], rng: scala.util.Random): Int = 
    interval.lower + rng.nextInt(1 + interval.length)

  def selectUniform(interval: ClosedInterval[Double], rng: scala.util.Random): Double = 
    interval.lower + ( rng.nextDouble() * ( 1.0 + interval.length ) )
}

///////////////////////////////////

case class ClosedOpenInterval[T<%Ordered[T]](lower: T, upper: T) extends Ordered[ClosedOpenInterval[T]] {
  
  require( lower < upper, s"Required valid ClosedOpenInterval, found [$lower,$upper)" )

	///////////////////////////////

  def contains(x: T): Boolean = 
    lower <= x  && x < upper

  def length(implicit ev: AdditiveGroup[T]): T = 
    ev.minus(upper,lower)
    
  def translateBy(x: T)(implicit ev: AdditiveAbGroup[T]): ClosedOpenInterval[T] = 
    ClosedOpenInterval( ev.plus(lower,x),ev.plus(upper,x))

  def constrain(x: T): T = 
    if( x < lower ) lower else if( x >= upper ) upper else x 
    
	///////////////////////////////    

  def overlaps(other: ClosedOpenInterval[T]): Boolean = 
    contains( other.lower ) || ( contains( other.upper ) && other.upper != lower )

  def contains(other: ClosedOpenInterval[T]): Boolean = 
    contains( other.lower ) && other.upper <= upper

  def isBefore(other: ClosedOpenInterval[T]): Boolean =
    upper < other.lower

  def isAfter(other: ClosedOpenInterval[T]): Boolean =
    lower >= other.upper

  def intersection(other: ClosedOpenInterval[T]): Option[ClosedOpenInterval[T]] = {
    val l = if(lower > other.lower) lower else other.lower
    val u = if(upper < other.upper) upper else other.upper    
    if( l < u ) Some(ClosedOpenInterval(l,u)) else None
  }
    
	///////////////////////////////
    
  def map[U<%Ordered[U]](f: T => U)(implicit ev: Ordered[U]): ClosedOpenInterval[U] = 
    ClosedOpenInterval(f(lower),f(upper))  

//  def after(other: ClosedOpenInterval[T]): ClosedOpenInterval[T] = { 
//    if(isAfter(other)) this else ClosedOpenInterval(other.upper, ev.plus(other.upper,length) )  
//  } ensuring { _.isAfter(other) }
//  
    
  /////////////////////////////////
  
  override def toString(): String = s"[ $lower, $upper)"

  override def compare(rhs: ClosedOpenInterval[T]): Int = 
    if( lower < rhs.lower ) -1 else { 
      if( lower > rhs.lower ) 1 else {
        if( upper < rhs.upper ) -1 else { 
          if( upper == rhs.upper ) 0 else 1 }}}
}

object ClosedOpenInterval { 
  
}

// End ///////////////////////////////////////////////////////////////
