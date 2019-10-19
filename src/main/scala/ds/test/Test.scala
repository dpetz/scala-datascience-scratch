package ds.test

import ds.num.Real

object Test {


  type E[T] = Expr[T]
  type S[T] = Seq[T]
  type M[T] = Matrix[T]

  trait Engine {
    def apply[T](e:E[T]):T
  }

  abstract class Matrix[R] {
    def rows:Seq[Seq[R]]
    def columns:Seq[Seq[R]]
  }

  /** Monadic with ``expr`` as unit and  ``next`` as flatMap */
  case class Expr[X](inputs:Product)(eval:Engine=>X) {

    def **[Y,Z](y:E[X])(implicit op:TimesTimes[X,Y,Z]):E[X] = lift(op)(this,y)

    def *[Y,Z](y:E[Y])(implicit op:Times[X,Y,Z]):E[Z] = lift(op)(this,y)

    def /[Y,Z](y:E[Y])(implicit op:Div[X,Y,Z]):E[Z] = lift(op)(this,y)

    // For Scala for comprehensions
    def flatMap[Y](f:X=>E[Y]):E[Y] = Test.next(this,f)

    def map[Y](f:X=>Y):E[Y] = Test.map(this,f)


  }

  /*
  Design Considerations

  Functions should output expressions.
  Otherwise they cannot leverage any expression syntax internally
  because they have no engine to evaluate before they return.

  Should their inputs be expressions? Two possible answers:

  No, because it means unnecessary boxing of results before passing into the next expression. Also, by
  accepting raw types they fit flatMap perfectly

  Yes, because otherwise callers with expressions cannot use the function unless they have an engine handy.
  But wait, they could flatMap in this case. But only if function takes a single input ...

   */



  /** Namespace */
  def id(s:String):String = s"${this.getClass.getName}.$s"

  /** flatMap */
  def next[X,Y](expr:E[X], f:X=>E[Y]): E[Y] = Expr(id("next"),expr,f) {
    e => e(f(e(expr)))
  }

  implicit def expr[T](t:T): E[T] = Expr(None) { _ => t }

  trait Plus[X,Y, Z] extends ((X,Y)=>Z)
  implicit def realPlus[R](implicit real:Real[R]): Plus[R, R, R] = real.plus

  trait Div[X,Y,Z] extends ((X,Y)=>Z)
  implicit def realDiv[R](implicit real:Real[R]): Div[R, R, R] = real.div

  /** Raises ``Real`` to the power of another ``Real``.
    * Matrix multiplication also implements ``Power`` to inherit its symbol ``**`` */
  trait TimesTimes[X,Y,Z] extends ((X,Y)=>Z)
  implicit def power[R](implicit real:Real[R]):TimesTimes[R,R,R] = real.power
  implicit def matrixTimesMatrix[R](implicit real:Real[R]):TimesTimes[M[R],M[R],M[R]] =
    (m1: Matrix[R], m2: Matrix[R]) =>
      m1.rows map (m1_row => m2.columns map (m2_col => dot(m1_row, m2_col )))

  trait Times[X,Y,Z] extends ((X,Y)=>Z)
  implicit def vecTimes[R](implicit real:Real[R]): Times[S[R],S[R],S[R]] =
    (v: S[R], w: S[R]) => elementwise[R](real.times)(v, w)
  `



  def elementwise[R](f:(R,R)=>R): (S[R],S[R])=>S[R] =
    (v:Seq[R],w:Seq[R]) => (v zip w) map (x => f(x._1,x._2))

  /** Lift ``Function2`` to transform from/to ``Expr`` */
  def lift[X1,X2,Y](f:(X1,X2)=>Y):(E[X1],E[X2])=>E[Y] =
    (e1, e2) => Expr(e1,e2) {e => f(e(e1), e(e2)) }

  /** Lift ``Function1`` to transform from/to ``Expr`` */
  def lift[X,Y](f:X=>Y):E[X]=>E[Y] =
    e1 => Expr(e1) {e => f(e(e1)) }

  def sum[R](v:E[Seq[R]])(implicit real:Real[R]):E[R] =
    Expr(v) { e=> e(v).foldLeft(real.zero)(real.plus) }

  def mapSeq[S,T](v:E[Seq[S]],f:Expr[S]=>Expr[T]):E[Seq[T]] =
  Expr(v,f) { e => e(v) map (x => e(f(expr(x)))) }

  def map[S,T](v:E[S],f:S=>T):E[T] =
    Expr("map",v,f) { e => f(e(v)) }

  def distribute[S](v:E[Seq[S]]):Seq[E[S]]

  def dot[R:Real](v:E[Seq[R]],w:E[Seq[R]]): E[R] = sum( v * w )

  def norm[R](p:Int)(v:E[Seq[R]])(implicit real:Real[R]):Expr[R] =  p match {
    case real.one =>  sum (map(v, real.abs))
    case p:_ => sum( distribute(v).map( andThen (expr(_) ** real(p) ) ** real.one./(real(p))

  }




}
