package ds.lina

import parser.Json
import scala.util.{Try, Success, Failure}


/**
  * Minimal interface for a matrix. 
  */
trait Matrix[A] {
   

  /** Number of rows */
  def rows:Int
  /** Number of columns */
  def columns:Int

  /** Gets entry by index*/
  def apply(i:Int,j:Int):A

  override def equals(other:Any) =
    other.isInstanceOf[Matrix[A]] && (hashCode == other.hashCode)

  override def hashCode = (this elements).hashCode + rows

  override def toString = s"Matrix($rows rows, $columns‚ columns)"

}

object Matrix {

  /** Implements [[Matrix]] as a vector (the rows) of vectors (the entries).  */
  case class VecOfRowVecs[A](data:Seq[Seq[A]]) extends Matrix[A] {

    /** Checks all row vectors have equal length */
    require (data.forall( _.size == columns),
      s"$columns elements expected: ${data.find(_.size != columns).get}"
    )

    def apply(i:Int,j:Int)=data(i)(j)
    def rows=data.size
    def columns=data(0).size

  }

  /** Copies [[Json]] array of number arrays into scala vector of vectors */
  def apply[A](json:Json,parse:Json=>A):Try[Matrix[A]]= {
    try {
        Success(VecOfRowVecs[A](
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

  /** Parses matrix of doubles from json string.
   *  Shortcut to ``apply(Json(jsonStr),parseDouble)´´. */
  def apply(jsonStr:String):Matrix[Double]=
    apply[Double](Json(jsonStr),parseDouble).get 

}

