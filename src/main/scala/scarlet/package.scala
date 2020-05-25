
package object scarlet {
  
  /**
   * @see http://stackoverflow.com/questions/7852471/idiomatic-construction-to-check-whether-a-collection-is-ordered
   */
  
  def isSorted[T](l: Seq[T])(implicit ord: Ordering[T]): Boolean = 
    (l, l.tail).zipped.forall(ord.lt(_, _))  
}

// End ///////////////////////////////////////////////////////////////
