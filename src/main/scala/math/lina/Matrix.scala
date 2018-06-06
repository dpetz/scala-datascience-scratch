package math.lina

import io.Json
import scala.util.{Try, Success, Failure}




/**
  * Subclasses must implement at least [[rows]]
  */
trait Matrix[A] {
   

  /** Number of rows */
  def n:Int
  /** Number of columns */
  def m:Int
  def apply(i:Int,j:Int):A

  def row(i:Int):Row[A]=Row(i,this)
  def rows[B](f:Row[A]=>B):Seq[B]=(0 to n-1).map(Row(_,this)).map(f)

  def col(j:Int):Column[A]=Column(j,this)
  def cols[B](f:Column[A]=>B):Seq[B]=(0 to m-1).map(Column(_,this)).map(f)


  override def toString = s"Matrix($n rows, $m columns)"
}



object Matrix {

  /** Implements [[Rows]] as a vector (the rows) of vectors (the entries).  */
  case class SeqOfRows[A](val data:Seq[Seq[A]]) extends Matrix [A] {

    /** Checks all row vectors have equal length */
    require (data.forall( _.size == m),
      s"$m elements expected: ${data.find(_.size != m).get}"
    )

    def apply(i:Int,j:Int)=data(i)(j)
    def n=data.size
    def m=data(0).size


  }

  
 /** Copies [[Json]] array of number arrays into scala vector of vectors */
  def fromJson[A](json:Json,parse:Json=>A):Try[Matrix[A]]= {
    try {
        Success(new SeqOfRows[A](
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