import io.parse._
import io._
import util._


object Main extends App {

val json = Json(
    """{
      |  "Eins": 1,
      |  "Zwei": 2,
      |  "Object": {
      |    "Drei": 3,
      |    "Array": [4,5,[]]
      |  }
      |}
    """.stripMargin
)

println(json.tree.leaves.mkString(" | "))
  	
}