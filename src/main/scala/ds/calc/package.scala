package ds

// https://github.com/scalanlp/breeze/wiki/Linear-Algebra-Cheat-Sheet

package object calc {

  import ds.algebra._
  import ds.lina._

  /** https://en.wikipedia.org/wiki/Scalar_field */
  type ScalarField = Function1[Vec,Double]
  /** https://en.wikipedia.org/wiki/Vector_field */
  type VectorField = Function1[Vec,Vec]

}
