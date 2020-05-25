package scarlet.collection.immutable

object MultiSet {
 
  def apply[T](x: Seq[T]): Map[T,Int] =
    x.groupBy { x => x }.map { case (k,vs) => k -> vs.size }    
  
  ///////////////////////////////
    
  def isValid[T](x: Map[T,Int]): Boolean =
    x.values.forall { _ > 0 }
    
  def contains[T](set: Map[T,Int], subset: Map[T,Int]): Boolean =  
    subset.forall { case (k,count) => set.get(k).exists { c => count <= c } }
  
  def union[K](lhs: Map[K, Int], rhs: Map[K, Int]): Map[K, Int] =
    lhs.foldLeft(rhs) {
      case (acc, (k, v)) => acc.updated(k, math.max(v, acc.get(k).getOrElse(0)))
    }    

  def intersection[K](lhs: Map[K, Int], rhs: Map[K, Int]): Map[K, Int] =
    lhs.foldLeft(rhs) {
      case (acc, (k, v)) => acc.updated(k, math.min(v, acc.get(k).getOrElse(0)))
    }    
  
  def sum[K](lhs: Map[K, Int], rhs: Map[K, Int]): Map[K, Int] =
    lhs.foldLeft(rhs) {
      case (acc, (k, v)) => acc.updated(k, v + acc.get(k).getOrElse(0))
    }    
}

// End ///////////////////////////////////////////////////////////////
