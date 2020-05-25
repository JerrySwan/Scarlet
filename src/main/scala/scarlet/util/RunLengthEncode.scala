package scarlet.util

object RunLengthEncode {

	def apply[T](in: Seq[ T ]): IndexedSeq[( T, Int )] = 
	  apply(in, (a: T,b: T) => a == b )
	  
  /////////////////////////////////
	
	def apply[T](in: Seq[ T ], eq: (T,T) => Boolean): IndexedSeq[( T, Int )] = {
		var result = Vector[ ( T, Int ) ]()

		if( !in.isEmpty ) {
			var current = in( 0 )
			var currentLen = 1
			
			for( i <- 1 until in.size ) {
				if( !eq(in( i ), current ) ) {
					result = result :+ ( current, currentLen )
					current = in( i )
					currentLen = 0
				}
			   
				currentLen += 1
				result = result :+ ( current, currentLen )			   
			}
		}
		
		result		
	}
	
//	def pack[T](vals: List[T]): List[List[T]] = vals match {
//		case Nil => Nil
//		case x :: xs1 => 
//		  val (first, rest) = vals span (y => y == x)
//		  first :: pack(rest)
//	}
//	
//	// def encode[T](vals: List[T]): List[(T, Int)] =
//	def apply[T](vals: List[ T ]): List[( T, Int )] =	  
//	  pack(vals) map (ys => (ys.head, ys.length))	
}

// End ///////////////////////////////////////////////////////////////
