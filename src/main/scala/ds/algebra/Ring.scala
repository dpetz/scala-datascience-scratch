package ds.algebra

trait Ring[A] extends Group[A] {
  def one:A
  def times(l:A,r:A):A
  def times(lr:(A,A)):A=times(lr._1,lr._2)
}