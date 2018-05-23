package math.lina

import scala.collection.{AbstractSeq,AbstractIterator}

class ColView[A](val m:Matrix[A], val j:Int) extends AbstractSeq[A] {

  override def apply(i:Int):A = m(i,j)

  def iterator:Iterator[A] = new ColIterator(m,j)


  def length:Int=m.nRows

  class ColIterator[A](m:Matrix[A], j:Int) extends AbstractIterator[A] {
	var i:Int = -1 
  	def hasNext:Boolean = (i+1) < m.nRows
  	def next():A = {
        i+=1
        m(i,j)
    }
  }

}