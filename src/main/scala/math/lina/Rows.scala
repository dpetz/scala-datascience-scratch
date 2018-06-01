package math.lina

import io.Json
import scala.util.{Try, Success, Failure}
import util.SeqWrapper
import scala.collection.AbstractSeq


/** Represents a [[Matrix]] as [[Seq]] of rows. */
trait Rows[A] extends Matrix[A] with Seq[Seq[A]]


object Rows {

  /** Implements [[Rows]] as a vector (the rows) of vectors (the entries).  */
  class VecOfVec[A](override val toSeq:Seq[Seq[A]]) extends SeqWrapper[Seq[A]] with Rows[A] {

    /** Checks all row vectors have equal length */
    require (toSeq.forall( _.size == toSeq(0).size),
      s"${toSeq(0).size} elements expected: ${rows.find(_.size == toSeq(0).size).get}"
    )


  }

  def apply[A](mat:Matrix[A]):Rows[A]=new WrapMatrix(mat)

  class WrapMatrix[A](mat:Matrix[A]) extends SeqWrapper[Seq[A]] with Rows[A] {

      override val toSeq = (0 to mat.columns(0).size-1).map { new Row(this,_) }

      override def columns=mat.columns
      override def entries=mat.entries

  }

    /** Wraps column as [[Seq]] */
  class Row[A](val m:Matrix[A], val i:Int) extends AbstractSeq[A] {
    def apply(j:Int):A = m(i,j)
    def iterator:Iterator[A] = (0 to length-1).map(m(i,_)).iterator
    def length:Int=m.columns.size
  }


 /** Copies [[Json]] array of number arrays into scala vector of vectors */
  def fromJson[A](json:Json,parse:Json=>A):Try[Rows[A]]= {
    try {
        Success(new VecOfVec[A](
          json.toArr.get.values.map {  // rows              
                _.toArr.get.values.map { // row
                  parse(_)      // values
                }.toVector
          }.toVector
        ))
    } catch {
        case e:Exception => Failure(e)
    }
  }


  def parseDouble(json:Json):Double = json.toNum.get.value

  /** Shortcut to ``fromJson(Json((jsonStr))´´. */
  def apply(jsonStr:String):Matrix[Double]=
    fromJson[Double](Json(jsonStr),parseDouble).get
}

