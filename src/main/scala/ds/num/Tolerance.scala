package ds.num

trait Tolerance[A] {
  def epsilon:A
  def approx(x:A,y:A):Boolean
}
