package scarlet.numeric

import breeze.linalg._
import breeze.numerics._

///////////////////////////////////

final case class MarkovChain(transitions: DenseMatrix[Double]) {
  require( transitions.rows == transitions.cols )
  require( MarkovChain.isRightStochastic( transitions ) )
  
  val rows = transitions.rows
  val cols = transitions.cols  
  
  def apply(i: Int, j: Int): Double = transitions(i,j)
}

///////////////////////////////////

object MarkovChain {

  def sumsToOne(v: DenseVector[Double]): Boolean = 
    breeze.numerics.closeTo(v.toVector.sum,1.0)
  
  def isStochastic(v: DenseVector[Double]): Boolean =   
    v.forall { x => 0 <= x && x <= 1.0 } && sumsToOne( v )
    
  def isLeftStochastic(m: DenseMatrix[Double]): Boolean =
    ( 0 until m.cols ).forall { i => isStochastic( m(::,i) ) }

  def isRightStochastic(m: DenseMatrix[Double]): Boolean = 
    isLeftStochastic(m.t)

  def isStochastic(m: DenseMatrix[Double]): Boolean =
    isRightStochastic(m) || isLeftStochastic(m)
  
  def isDoublyStochastic(m: DenseMatrix[Double]): Boolean =
    isRightStochastic(m) && isLeftStochastic(m)

  def isErgodic(transitions: DenseMatrix[Double]): Boolean = {
    import breeze.linalg._, eig.Eig, breeze.math._
    val Eig(eigenvalues, eigenvaluesComplex, eigenvectors) = eig(transitions)
    eigenvalues.toArray.forall { _ >= 0 } && 
      breeze.numerics.closeTo(1.0,eigenvalues.toArray.sorted.last)  
  }
    
  /////////////////////////////////
  
  def toStochasticVector(v: DenseVector[Double]): DenseVector[Double] = {
    v.map { x => x / v.sum }
  } ensuring { isStochastic(_) }

  def toRightStochasticMatrix(m: DenseMatrix[Double]): DenseMatrix[Double] = {
    m(*, ::).map { toStochasticVector } 
  } ensuring { isRightStochastic(_) }
  
  /////////////////////////////////  
    
  def multisetToStochasticVector(m: Map[Int,Int], numStates: Int): DenseVector[Double] = {
    require( m.values.forall { _ > 0 } )
    require(m.keys.forall { k => k >= 0 && k < numStates } )

    if( m.isEmpty ) 
      DenseVector.zeros[Double](numStates)      
    else { 
      val sum = m.values.sum
      DenseVector.tabulate(numStates) { i => m.getOrElse(i,0).toDouble / sum }
    }
  } ensuring { v => m.isEmpty || isStochastic(v) }
    
  /////////////////////////////////
    
  def stationaryDistributionEigendecomp(A: DenseMatrix[Double]): DenseVector[Double] = {
    
    // require( MarkovChain.isLeftStochastic( m ) )
    require( MarkovChain.isRightStochastic( A ) )
    
    val m = A.t
    
    import Jama.Matrix
    import Jama.EigenvalueDecomposition
    
    val N = m.rows
    val transition = Array.tabulate(N,N) { (i,j) => m(i,j) }
    
    val eig = new EigenvalueDecomposition(new Matrix(transition))
    val V = eig.getV()
    val real = eig.getRealEigenvalues()

    // find eigenvector corresponding to eigenvalue = 1
    
    assert( real.toList.count { breeze.numerics.closeTo(_,1.0,1e-4) } >= 1 )
    
    val i = real.toList.indexWhere { breeze.numerics.closeTo(_,1.0) }
    var x = V.getMatrix(0, N-1, i, i)
    x = x.times(1.0 / x.norm1() )
    DenseVector.tabulate(N) { j => x.get(j,0) }
  }
  
/*********************************************
  
	def stationaryDistributionPowerMethod(
    m: DenseMatrix[Double], maxPow: Int): Option[DenseVector[Double]] = {
    
    require( MarkovChain.isRightStochastic( m ) )

    	import cats._

    	implicit val ev = new Semigroup[DenseMatrix[Double]] { 
        override def combine(a: DenseMatrix[Double], b: DenseMatrix[Double]) = a * b
      }
    	
    (1 until maxPow).toStream.map { 
      i => ev.combineN(m, i ) // pow( m, i )
      }.dropWhile { mpow => 
        Breeze.allRowsEqual( mpow ) 
          }.headOption.map { m => m( 0, :: ).t }
  }
	
	/////////////////////////////////
	
  def entropyRatePowerMethod(m: DenseMatrix[Double], normalize: Boolean): Option[Double] = {
    require( MarkovChain.isRightStochastic(m) )    
    require( m.rows == m.cols )
    
    val numStates = m.rows
    
    def safeLog(m: DenseMatrix[Double]): DenseMatrix[Double] = 
      m.map { x => if( x == 0 ) 0 else log(x) }
    
    MarkovChain.stationaryDistributionPowerMethod(m, 50).map { steady =>
      val H = -( steady.t * m * safeLog(m) ).t.sum
      if( normalize ) H / (numStates * math.log(numStates)) else H
    }
  }
*********************************************/
  
  def entropyRateEigendecomp(m: DenseMatrix[Double], normalize: Boolean): Double = {
    require( MarkovChain.isRightStochastic(m) )    
    require( m.rows == m.cols )
    
    val numStates = m.rows
    
    def safeLog(v: DenseVector[Double]): DenseVector[Double] = 
      v.map { x => if( x == 0 ) 0 else log(x) }
    
    // Adapted from: https://github.com/riccardoscalco/Pykov/blob/master/pykov.py

    // H_i = \sum_j P_{ij} \ln P_{ij}
    // H = \sum \pi_i  H_i
    // Khinchin, A. I., Mathematical Foundations of Information Theory, Dover, 1957.    
    
    val p = MarkovChain.stationaryDistributionEigendecomp(m)
jeep.lang.Diag.println( p )    
    val H = ( for { state <- 0 until numStates } yield { 
      val v = m(state,::).t
      p(state) * ( v * safeLog(v) ).sum 
    } ).sum 
    if( normalize ) -H / (numStates * math.log(numStates)) else -H
  }
  
  def entropyRateEigendecomp2(m: DenseMatrix[Double], p: DenseVector[Double] //, normalize: Boolean
  ): Double = {
    require( MarkovChain.isRightStochastic(m) )    
    require( m.rows == m.cols )
    
    val numStates = m.rows
    
    def safeLog(v: DenseVector[Double]): DenseVector[Double] = 
      v.map { x => if( x == 0 ) 0 else log(x) }
    
    // Adapted from: https://github.com/riccardoscalco/Pykov/blob/master/pykov.py

    // H_i = \sum_j P_{ij} \ln P_{ij}
    // H = \sum \pi_i  H_i
    // Khinchin, A. I., Mathematical Foundations of Information Theory, Dover, 1957.    
    
    // val p = MarkovChain.stationaryDistributionEigendecomp(m)
    val H = ( for { state <- 0 until numStates } yield { 
      val v = m(state,::).t
      p(state) * ( v * safeLog(v) ).sum 
    } ).sum 
  //  if( normalize ) -H / (numStates * math.log(numStates)) else -H
    H
  }
  
}

// End ///////////////////////////////////////////////////////////////
