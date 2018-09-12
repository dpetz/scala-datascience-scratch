package math.algebra

trait Group[A] extends Monoid[A] {
	def negate(v:A):A
	def minus(l:A, r:A):A
}