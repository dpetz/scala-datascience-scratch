package ds.num
import ds.expr.{Engine, Relation}

case class Approx[R:Real](override val x:E[R],
                          override val y:E[R])
                         (implicit real:Real[R])
  extends Relation[R](x,y) {

    def apply(e:Engine): Boolean = real.approx(e(x),e(y))
}