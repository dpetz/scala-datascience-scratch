package ds.expr

import ds.num.Real
import ds.expr.Engine.Config
import ds.func.{Assign, Func, Symbol}


/** Engine for `Real` arithmetic*/
class SimpleEngine(private val configs:List[Config]=Engine.defaults,
                           private val vars:Map[Symbol[_],_] = Map.empty) extends Engine{

  def config[C <: Config](s:String):C = configs.find(_.name == s).asInstanceOf[C]

  def update(c:Config):Engine = new SimpleEngine(c :: configs, vars)

  def apply[A](e:Expr[A]): A = e match {
    case v:Symbol[A] => vars(v).asInstanceOf[A]
    case _ => e(this)
  }

  def apply[T](f:Func[T], x:T): T = (new SimpleEngine(configs, vars(f.x)=x))(f)

}


case class EngineException[R](eng:Engine[R], expr:Expr[R]) extends Exception {
  override def toString = s"Engine $this cannot evaluate $expr"
}
