package scarlet.collection.immutable

import algebra.instances.AllInstances

///////////////////////////////////

object Collections {

  def isSorted[T](l: Seq[T])(implicit ord: Ordering[T]): Boolean = 
    (l, l.tail).zipped.forall { ord.lt(_, _) }  
  
  import algebra.instances._
  
  def isConsecutive[T](l: Seq[T])(implicit ev: algebra.ring.Rig[T]): Boolean = 
    (l, l.tail).zipped.forall { case (a,b) => b == ev.plus( a, ev.one ) }
}

// End ///////////////////////////////////////////////////////////////
