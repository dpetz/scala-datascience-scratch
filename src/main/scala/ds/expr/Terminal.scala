package ds.expr

import ds.num.real.Real



case class Terminal[R](eval:()=>R) extends RealValued[R] (e => eval(e.real))