package math.lina
import util.SeqWrapper

import scala.collection.AbstractSeq


/** Represents a [[Matrix]] as [[Seq]] of columns. */
trait Columns[A] extends Matrix[A] with Seq[Seq[A]]

object Columns {


    def apply[A](mat:Matrix[A]):Columns[A]= new MatrixAsColumns(mat)

    class MatrixAsColumns[A](val mat:Matrix[A]) extends SeqWrapper[Seq[A]] with Columns[A] {
        override val toSeq = (0 to mat.rows(0).size-1).map { new Column(this,_) }

    override def rows=mat.rows
    override def entries=mat.entries
    }

    /** Wraps column as [[Seq]] */
  class Column[A](val m:Matrix[A], val j:Int) extends AbstractSeq[A] {
    def apply(i:Int):A = m(i,j)
    def iterator:Iterator[A] = (0 to length-1).map(m(_,j)).iterator
    def length:Int=m.rows.size
  }

}