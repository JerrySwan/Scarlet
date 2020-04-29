package scarlet.io

object File {

  def listFiles(f: java.io.File, recurse: Boolean): List[java.io.File] = {
    if( f.isFile ) 
      Nil
    else {
      f.listFiles.toList.map { ff => 
        if( recurse && ff.isDirectory ) listFiles(ff,recurse) else List(ff)   
      }.flatten
    }
  }
}

// End ///////////////////////////////////////////////////////////////
