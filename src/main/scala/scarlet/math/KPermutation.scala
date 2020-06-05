package scarlet.math

///////////////////////////////////

final class KPermutationIterator(n: Int, k: Int) extends Iterator[List[Int]] {
  require( n >= k )
  private val impl = new jcombinatorics.permutations.SepaPnkIterator( n, k )
  
  override def hasNext: Boolean = impl.hasNext
  override def next(): List[Int] = {
    val nxt = impl.next()
    nxt.toList
  } ensuring { result => result.length == k && result.forall { x => x >= 0 && x < n } }
}
  
/////////////////////////////////  

final class KPermutationWithReplacementIterator(n: Int, k: Int) extends Iterator[List[Int]] {
  private val impl = new jeep.util.Pascaline( k, 0, n )
  
  override def hasNext: Boolean = impl.hasNext
    
  override def next(): List[Int] = {
    impl.next()
      (0 until k).toList.map { i => impl.get(i) }.reverse
    } ensuring { result => result.length == k && result.forall { x => x >= 0 && x < n } }
}

// End ///////////////////////////////////////////////////////////////
