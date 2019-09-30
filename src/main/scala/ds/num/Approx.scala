package ds.num
import ds.expr.Engine

case class Approx[R:Real](x:E[R],y:E[R])(implicit real:Real[R])  extends E[Boolean] {
    def apply(e:Engine): Boolean = real.approx(e(x),e(y))
}