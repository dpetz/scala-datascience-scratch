package ds.expr

import ds.num.Real._
import ds.expr.Engine.{Config, Layout}
import ds.lina.Vec


/** Engine for `Real` arithmetic*/
class RealEngine[R:Real](private val configs:List[Config]=Engine.defaults)(implicit real:Real[R]) extends Engine[R]{

  def config[C <: Config](s:String):C = configs.find(_.name == s).asInstanceOf[C]

  def update(c:Config):Engine[R] = new RealEngine(c :: configs)

  def apply[A](e:Expr[R,A]): A = e(this)

}


case class EngineException[R](eng:Engine[R], expr:Expr[R]) extends Exception {
  override def toString = s"Engine $this cannot evaluate $expr"
}
