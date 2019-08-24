package ds.algebra

trait Group[A] extends Monoid[A] {
	def negate(v:A):A=minus(zero,v)
	def minus(l:A, r:A):A
}