package scarlet.numeric

import breeze.linalg._
import breeze.math._
import breeze.util.Implicits._
import breeze.numerics._
import breeze.linalg.{isClose, Vector}
// import breeze.util.Implicits._
// import breeze.linalg.operators._
// import breeze.generic.UFunc
// import breeze.linalg.BroadcastedRows.BroadcastRowsDMToIndexedSeq

///////////////////////////////////

object Breeze {
 // dm(*, ::).map { dv => } maps rows
// dm(::, *).map { dv => } // maps columns

  def allRowsEqual(m: DenseMatrix[Double]): Boolean = { 
    val rowSeq = m.t(::,*).toIndexedSeq
    rowSeq.isEmpty || rowSeq.tail.forall { v => breeze.linalg.isClose(v,rowSeq.head) }
  }
  
  def toString[T](m: Matrix[T]): String = {
    val sb = new StringBuffer()
    sb.append("[")
    for( i <- 0 until m.rows ) {
      sb.append("[")      
      for( j <- 0 until m.cols ) {
        sb.append( m(i,j) )
        if( j < m.cols - 1 )
          sb.append( "," )                  
      }
      sb.append("]")
      if( i < m.rows - 1 )
        sb.append( ",\n" )                  
    }
    sb.append("]")    
    sb.toString
  }
}

// End ///////////////////////////////////////////////////////////////
