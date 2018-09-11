/**
  def test(dim:Int=3,dist:Double=10) {

    println(s"Descending $dim-dimensional v*v. Random start coordinates in [-$dist,$dist) ...")

    val min = minimize(
      Gradient(
        _.map(x=>x*x).sum,                          // function
        (v,i) => 2*v(i)),                           // ith derivative
      Seq.fill(dim)(Random.nextDouble*2*dist-dist)) // start random

    println("i\tValue\tPosition\n" + "="*30)
    min.foreach {
      gd => printf("%s\t%.4f\t",gd.count,gd.value)
        println(format(gd.pos,"%.4f"))
    }
    printf("Expecting value 0 at position (0"+",0"*dim+")")


  }
  */