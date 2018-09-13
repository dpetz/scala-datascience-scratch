import org.scalatest._
import ds.lina._

class Matrix extends FlatSpec with OptionValues with Matchers {

	val matrix = Matrix("[[1,2],[3,4]]")

	"M=[[1,2],[3,4]]" should "(M + M) equal (2 * M)" in {
		(matrix + matrix) should equal (matrix * 2)
	}

	it should "(M * M) elements sum equal 54" in {
		((matrix * matrix elements) sum) should equal (54) 
	}
}