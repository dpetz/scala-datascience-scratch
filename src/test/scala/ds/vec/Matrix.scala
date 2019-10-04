
import ds.matrix.Matrix
import ds.vec._
import ds.num.DoubleReal._
import ds.matrix.Matrix.Math
import ds.vec.VecInfix._

class Matrix extends ds.SimpleSpec {

	val matrix = Matrix[Double]("[[1,2],[3,4]]")

	"M=[[1,2],[3,4]]" should "(M + M) equal (2 * M)" in {
		(matrix + matrix) should equal (matrix * 2)
	}

	it should "(M * M) elements sum equal 54" in {
		((matrix * matrix all) sum) should equal (54)
	}
}