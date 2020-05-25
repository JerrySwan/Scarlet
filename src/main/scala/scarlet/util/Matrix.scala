package scarlet.util

case class Matrix[T](values: List[List[T]]) {
  require( !values.isEmpty )  
  require( values.tail.forall { _.length == values.head.length } )
  
  val size1 = values.length
  val size2 = values.head.length
  
  def apply(i: Int, j: Int): T = values(i)(j)
  
  def map[U](f: T => U): Matrix[U] = 
    Matrix( values.map { _.map { f } } )
  
  override def toString(): String =
    values.map{ _.mkString( "[",",","]" ) } . mkString("[",",","]")
}

// End ///////////////////////////////////////////////////////////////
