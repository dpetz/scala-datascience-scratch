import io.parse._
import io._


object Main extends App {

println(
Json(
    """{
      |  "Herausgeber": "Xema",
      |  "Nummer": "1234-5678-9012-3456",
      |  "Deckung": 2e+6,
      |  "Waehrung": "EURO",
      |  "Inhaber":
      |  {
      |    "Name": "Mustermann",
      |    "Vorname": "Max",
      |    "maennlich": true,
      |    "Hobbys": [ "Reiten", "Golfen", "Lesen" ],
      |    "Alter": 42,
      |    "Kinder": [],
      |    "Partner": null
      |  }
      |}
    """.stripMargin
).asInstanceOf[Obj]
)
//	println(Json(""" { "array"   : [1,2,3], "pi"    : 3.14, "true?":truue } """))

	//println(Json("truue"))
  	//println(Json.pArr(Json.Test.list))
  	//Parser.Test.and
  	//println(Json.pArr(Json.Test.matrix))
  	
}