package scarlet.util

object Random {

  def proportionalSelection[T](xs: List[T], numSelections: Int, f: T => Double, random: scala.util.Random): List[T] = {
    require( !xs.isEmpty )
    
    val values = xs.map { f }
	  val cumulativeFitness = values.tail.scanLeft(values.head) { _ + _ }.toArray
	  
    if( cumulativeFitness.last == 0.0 )
      for( i <- ( 0 until numSelections ).toList ) yield { xs(random.nextInt( xs.length )) }
    else {
		  for( i <- ( 0 until numSelections ).toList ) yield {
			  val randomFitness = random.nextDouble() * cumulativeFitness.last
			  val index = java.util.Arrays.binarySearch( cumulativeFitness, randomFitness )
			  if( index >= 0 ) xs(index) else xs(math.abs( index + 1 ))
		  }
    }
  }  
}

// End ///////////////////////////////////////////////////////////////
