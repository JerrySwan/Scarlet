package scarlet.util

object SeqUtils {

  def consecutiveEncode(l: List[Int]): List[(Int,Int)] = {
    if(l.isEmpty) 
      Nil 
    else {
      l.tail.scanLeft((l.head,1)) { case ((value,count),newValue) =>
        if( newValue == value + count ) (value,count + 1) else (newValue,1)
      }.groupBy { _._1 }.map { case (k,v) => v.last }.toList.sortBy { _._1 }
    }
  }
  
  def main(args: Array[String]): Unit = {
    println( consecutiveEncode( List(1,2, 4) ) )
    // println( consecutiveEncode( List(1,2, 4) ).groupBy { _._1 }.map { case (k,v) => v.last }.toList.sortBy { _._1 }  )
  }
  
//  def consecutiveEncode(l: List[Int]): List[(Int,Int)] = l.tail.scanLeft((l.head,1)) { case (value,length) => 
//    ???
//  }
//  match {
//    case Nil => Nil
//    case x :: y :: xs => if( y == x + 1 ) ??? else ???
//    case x :: xs => Nil    
//  }
  
  def partialCombine[T](in: Seq[T], merge: (T,T) => Option[T]): Seq[T] = {
		in.tail.foldLeft( List(in.head) ) { case (list,x) =>
		  merge(list.last, x) match {
		    case Some(merged) => list.dropRight(1) :+ merged
		    case None => list :+ x		    
		  }
		}
	}
}

// End ///////////////////////////////////////////////////////////////
