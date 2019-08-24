package ds.algebra

trait Field[A] extends Ring[A] {
  def div(l:A, r:A):A
  def inverse(v:A):A = div(one,v)
}