package math.algebra

trait Monoid[A] {
  def zero:A
  def plus(l:A, r:A):A
  def plus(lr:(A,A)):A = plus(lr._1,lr._2)
 }