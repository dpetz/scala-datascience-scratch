package ds.test

import ds.num.Real
import ds.test.Test.TimesTimes

/**
  === Design Considerations ===

  Functions should output expressions.
  Otherwise they cannot leverage any expression syntax internally
  (such as dot notation in a matrix multiplication or power syntax in norm)
  because they have no engine to evaluate before they return.

  Should their inputs be expressions?

  Possibilities:

  (A) No, because it requires unnecessary boxing of results before passing into subsequent expressions. Also, by
  accepting raw types they fit flatMap perfectly

  (B) Yes, because (1) otherwise callers with expressions cannot use the function unless they have an engine handy.
  But wait, they could flatMap in this case. But what if function takes several input?
  And (2) we want to bind arguments when constructing the expression. So we would lift (and thus box) anyway
  such as in:
  {{{
   def *[Y,Z](y:E[Y])(implicit op:Times[X,Y,Z]):E[Z] = lift(op)(this,y)
  }}}
  But what about functions that are inputs to other functions like in
  {{{
    def map[S,T](v:E[S],f:S=>T):E[T] = Expr("map",v,f) { e => f(e(v)) }
  }}}
  The above is bad because we cannot unlift expression functions (such as ``_ ** p`` in ``norm``) because
  we cannot unlift them w/o an engine.

  Are functions expressions?

  (A) Yes, because the whole reason to get this started was to have my algorithms be expressed via
  traversable combinators for inspection and injection.

  (B) But wait, the function application could be an expression instead the function itself. This would allow us
  integrate existing functions easily which makes the API more generic and usable. But we tried this before
  and ended up with many classes to bind various number of arguments without the nice Scalar syntax sugar such as ``=>``  .

  The solution is that functions or Scala Functions that when bound produce an expression. Consider:
  {{{
  def dot[R:Real](v:E[S[R]],w:E[S[R]]): E[R] = sum( v * w )
  }}}

  Here the infix ``*`` leverages a function that knows nothing about expressions:
  {{{
    implicit def vecTimes[R](implicit real:Real[R]): Times[S[R],S[R],S[R]] =
    (v: S[R], w: S[R]) => elementwise[R](real.times)(v, w)
  }}}
  By lifting it to a non-strict version that when bound creates an expression that holds
  the non evaluated arguments and the function until it gets an engine at its evaluation to execute everything.

  {{{
    /** Lift ``Function2`` to transform from/to ``Expr`` */
    def lift[X1,X2,Y](f:(X1,X2)=>Y):(E[X1],E[X2])=>E[Y] =
      (e1, e2) => Expr(e1,e2) {e => f(e(e1), e(e2)) }
  }}}

  To conclude our functions are ((Expr[_],...)=>Expr[_]) and we provide lift methods like above to integrate
  functions not operating on expressions.

   */
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

  object Matrix {
    def apply[R](data:Seq[Seq[R]]):M[R] = throw UnsupportedOperationException
  }

  /** Monadic with ``expr`` as unit and  ``next`` as flatMap */
  sealed trait Expr[X] {

    def eval:Engine=>X

    def **[Y,Z](y:E[Y])(implicit op:TimesTimes[X,Y,Z]):E[Z] = op(this,y)

    def *[Y,Z](y:E[Y])(implicit op:Times[X,Y,Z]):E[Z] = lift(op)(this,y)

    def /[Y,Z](y:E[Y])(implicit op:Div[X,Y,Z]):E[Z] = lift(op)(this,y)

    // For Scala for comprehensions
    def flatMap[Y](f:X=>E[Y]):E[Y] = Test.next(this,f)

    def map[Y](f:X=>Y):E[Y] = Test.map(this,lift(f))

  }

  case class Const[T](value:T) extends Expr[T] {
    def eval: Engine => T = _ => value
  }

  /** Cannot assume ``List[ Expr[_] ]`` because arguments might include functions (eg. map) */
  type Args = Product

  /** Good design to include the identifier in the arguments? */
  case class Term[T](args:Args)(override val eval:Engine=>T) extends Expr[T]
    //@todo: def mapArgs(f:Args=>Args):Term[T]


  case class Sym[T](id:String) extends Expr[T] {
    def eval: Engine => T = throw new UnsupportedOperationException
    // @todo implement Sym.eval
  }

  /** Namespace */
  def id(s:String):String = s"${this.getClass.getName}.$s"

  /** flatMap */
  def next[X,Y](expr:E[X], f:X=>E[Y]): E[Y] = Term(id("next"),expr,f) {
    e => e(f(e(expr)))
  }

  implicit def expr[T](t:T): E[T] = Const(t)

  trait Plus[X,Y, Z] extends ((X,Y)=>Z)
  implicit def realPlus[R](implicit real:Real[R]): Plus[R, R, R] = real.plus

  trait Div[X,Y,Z] extends ((X,Y)=>Z)
  implicit def realDiv[R](implicit real:Real[R]): Div[R, R, R] = real.div

  /** Raises ``Real`` to the power of another ``Real``.
    * Matrix multiplication also implements ``Power`` to inherit its symbol ``**`` */
  trait TimesTimes[X,Y,Z] extends ((E[X],E[Y])=>E[Z])

  implicit def power[R](implicit real:Real[R]):TimesTimes[R,R,R] =
    (ex, ey) => lift[R, R, R](real.power)(ex, ey)

  implicit def matrixTimesMatrix[R](implicit real:Real[R]):TimesTimes[M[R],M[R],M[R]] =
    (em1, em2) => Term("matrixTimesMatrix",em1,em2) { e =>
      Matrix(e(em1).rows map (m1_row => e(em2).columns map (m2_col => e(dot(m1_row, m2_col)))))
    }

  trait Times[X,Y,Z] extends ((X,Y)=>Z)
  implicit def vecTimes[R](implicit real:Real[R]): Times[S[R],S[R],S[R]] =
    (v: S[R], w: S[R]) => elementwise[R](real.times)(v, w)

  def elementwise[R](f:(R,R)=>R): (S[R],S[R])=>S[R] =
    (v:Seq[R],w:Seq[R]) => (v zip w) map (x => f(x._1,x._2))

  /** Lift `Function2` to transform from/to ``Expr`` */
  def lift[X1,X2,Y](f:(X1,X2)=>Y):(E[X1],E[X2])=>E[Y] =
    (e1, e2) => Term("lift",e1,e2) {e => f(e(e1), e(e2)) }

  /** Lift ``Function1`` to transform from/to ``Expr`` */
  def lift[X,Y](f:X=>Y):E[X]=>E[Y] =
    e1 => Term("lift",e1) {e => f(e(e1)) }

  def sum[R](v:E[Seq[R]])(implicit real:Real[R]):E[R] =
    Term("sum",v) { e=> e(v).foldLeft(real.zero)(real.plus)}

  def mapSeq[S,T](v:E[Seq[S]],f:Expr[S]=>Expr[T]):E[Seq[T]] =
  Term(v,f) { e => e(v) map (x => e(f(expr(x)))) }

  def map[S,T](x:E[S],f:E[S]=>E[T]):Term[T] = Term ("map",x,f) { e => e(f(x)) }

  //def distribute[S](v:E[Seq[S]]):Seq[E[S]]

  def dot[R:Real](v:E[S[R]],w:E[S[R]]): E[R] = sum( v * w )

  def norm[R](p:Expr[R], v:E[Seq[R]])(implicit real:Real[R]):Expr[R] =  p match {
    case real.one =>  sum (mapSeq(v, lift(real.abs)))
    case p:_ => sum( mapSeq(v, (x:E[R]) => x ** p)) ** ( real.one / p )

  }

}
