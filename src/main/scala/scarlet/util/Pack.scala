package scarlet.util

object Pack {
  
  def apply[A](l: List[A]): List[List[A]] = 
    apply(l, (a: A,b: A) => a == b )
  
  def apply[A](l: List[A], eq: (A,A) => Boolean ): List[List[A]] = {
    def _pack(res: List[List[A]], rem: List[A]): List[List[A]] = rem match {
        case Nil => res
        case h::tail if (res.isEmpty || !eq(res.last.head, h ) ) => _pack(res:::List(List(h)), tail)
        case h::tail => _pack(res.init:::List(res.last:::List(h)), tail)
    }
    _pack(List(),l)
  }  
}

// End ///////////////////////////////////////////////////////////////
