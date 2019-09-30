package ds.matrix

import ds.expr.Engine
import ds.expr.Engine.Columns
import ds.num.Real
import ds.vec.Vec._

/** Multiply matrices */
case class Times[R:Real](m1: Matrix[R], m2: Matrix[R])
  extends Matrix[R]() {

  val shape:Shape = Shape(m1.rows, m2.cols)

  def eval(e: Engine): Seq[Seq[R]] = {

    require(m1.shape.transpose == m2.shape,
      s"Cannot multiply $m1 and $m2: Shapes do not fit.")

    // in var names assume m1 has rows layout. Columns case symmetric
    val m2_cols = e.update(Columns())(m2)
    e(m1) map (m1_row => m2_cols map (m2_col => e(m1_row dot m2_col))) // @todo support filters

  }
}