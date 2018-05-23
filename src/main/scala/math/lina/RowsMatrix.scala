package math.lina

import io.Json
import scala.util.{Try, Success, Failure}

class RowsMatrix[A](val rows:Seq[Seq[A]]) extends Matrix[A] {

  require ( rows.forall( _.size == nCols),
    s"$nCols elements expected: " + rows.find(_.size == nCols).get
  )

}

object RowsMatrix {

  /** Copies [[Json]] array of number arrays into scala vector of vectors */
  def fromJson(json:Json):Try[RowsMatrix[Double]]= {
    try {
        Success(new RowsMatrix[Double](
        	json.toArr.get.values.map {  // rows              
                _.toArr.get.values.map { // row
                  _.toNum.get.value      // values
                }.toVector
        	}.toVector
        ))
    } catch {
        case e:Exception => Failure(e)
    }
  }

}

