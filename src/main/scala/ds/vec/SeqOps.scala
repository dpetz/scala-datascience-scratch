import scala.Function.tupled

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


