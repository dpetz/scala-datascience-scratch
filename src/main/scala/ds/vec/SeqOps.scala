import ds.num.Real

import scala.Function.tupled
package ds.vec

 class SeqOp[A](seq:Seq[A]) {

  /** View sequence as [[ds.matrix.Matrix]] with specified number of columns */
  //def align(cols: Int): Matrix[A] = Columnized(seq, cols)

  /** Convert to Json string. */
  def json: String = seq.mkString("[", ",", "]")

  /** Zip with indices and map to [[Elem]] */
  def indexed: Seq[Elem[A]] = seq.zipWithIndex map tupled { (x, i) => Elem(x, i) }

  case class Elem[A](x: A, i: Int)

  /** Returns random slices of given size.
    * Each element returned once except remainder which is ignored */
  def randomSlices(size: Int = 1): TraversableOnce[Seq[A]] = {
    import scala.util.Random
    val vShuffled = Random.shuffle(seq)
    for (b <- 0 until seq.length / size)
      yield vShuffled.slice(b * size, b * size + size)
  }

  val locale = java.util.Locale.US

  /** String interpolate vector elements */
  def format(ipol: String, start: String = "(", end: String = ")"): String = {
    (seq /:) (start) { (s:String, d:A) =>
      s + {
        if (s != start) "," else ""
      } + (ipol formatLocal(locale, d))
    } + end
  }
}

lazy val parser: Parser[Json.Arr] = Json.Parsers.arrOf(Json.Parsers.num)

/** Parse from json string */
def apply[R: Real](s: String)(implicit r: Real[R]): Vec[R] = seq2Vec(
Json(s, parser).toArr.values.map {
j: Json => r.json(j.toNum)
})

/** Parse sequence of doubles via [[Real.apply]] */
def apply[R](doubles: Seq[Double])(implicit real: Real[R]): Vec[R] =
doubles.map(real(_))
