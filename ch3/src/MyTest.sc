import MyList._
val a = List(1,2,3,4)
a
List.prod[Int](a, 1)(_ * _)
List.sum4[Int](a, 0)(_ + _)
List.prd4[Int](a, 1)(_ * _)
List.len4(a)
val x = List("x","y","z","w","q")
List.len4(x)
