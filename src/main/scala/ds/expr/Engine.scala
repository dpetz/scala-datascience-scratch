package ds.expr

import ds.lina.{Columns, Matrix, Rows, Vec}
import ds.num.real._

abstract class Engine[R:Real] {
  def apply(e:Expr[R]): R
  def apply(e:Vec[R]): Seq[R]
  def rows(e:Matrix[R]): Seq[Seq[R]]
  def cols(e:Matrix[R]): Seq[Seq[R]]


  /*
  def real:Real[R] = implicitly[Real[R]]
  def zero:R = real.zero
  def one:R = real.one
  def plus:(R,R)=>R = real.plus
  def times:(R,R)=>R = real.times
  def minus:(R,R)=>R = real.minus
  def div:(R,R)=>R = real.div
  */


}

