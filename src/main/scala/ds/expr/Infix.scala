package ds.expr

object Infix {

  type Binary[T] = (T,T)=>T
  type Relation[T] = (T,T)=>Boolean


  trait TimesTimes[T] extends ((T,T)=>T)
  trait Plus[T] extends ((T,T)=>T)
  trait Minus[T] extends ((T,T)=>T)
  trait Div[T] extends ((T,T)=>T)
  trait Times[T] extends ((T,T)=>T)

  trait Negate[T] extends (T=>T)

  trait Approx[T] extends ((T,T)=>Boolean)

  trait Compare[T] extends ((T,T)=>Int)


}
