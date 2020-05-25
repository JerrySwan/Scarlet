package scarlet.util

//////////////////////////////////////////////////////////////////////

case class Pascaline(state: Array[Int], bounds: Int => ClosedOpenInterval[Int]) { 
  require( invariant )

  /////////////////////////////////

  def isFinished: Boolean = 
    state.zipWithIndex.forall { case (x,i) => x + 1 == bounds(i).upper } 

  /////////////////////////////////
  
  def advance(): Unit = {  
    if( !isFinished ) { 
      var carry = true
      for( i <- 0 until state.size if carry ) {
        val index = state.size - i - 1
        state(index) += 1
        carry = state(index) == bounds(index).upper
        if( carry && index != 0 ) state(index) = bounds(index).lower 
      }
    }
    assert( invariant )
  }

  /////////////////////////////////
  
  override def toString = s"Pascaline(${state.mkString(",")})"
  
  def invariant: Boolean = 
    state.zipWithIndex.forall { case (x,i) => bounds(i).contains(x) }
}

///////////////////////////////////

class PascalineIterator(dim: Int, bounds: Int => ClosedOpenInterval[Int]) extends Iterator[List[Int]]{
  
  private val impl = new Pascaline(Array.tabulate(dim) { i => bounds(i).lower }, bounds)
  private var hasNext_ = true
  
  /////////////////////////////////
  
  override def hasNext = hasNext_
  
  override def next(): List[Int] = {
    require( hasNext )
    val result = impl.state.toList
    impl.advance()
    hasNext_ = impl.state.toList != result
    result
  }
}

///////////////////////////////////

object PascalineIterator {
  
  def fromUpperBounds(upperBounds: Seq[Int]): PascalineIterator = {
    require( upperBounds.forall { _ >= 0 } )
    new PascalineIterator(upperBounds.size, i => ClosedOpenInterval(0,upperBounds(i)))
  }
}

// End ///////////////////////////////////////////////////////////////
