import StdList._
val a = List(1,2,3,4,5)
a.take(2)
a.takeWhile(x => x>2)
a.takeWhile(x => x>1)
a.takeWhile(x => x<4)
a.forall(x => x>0)
a.exists(x => x<3)
StdList.Hello()
val b = "1" :: "2" :: List()
b match {
    case x :: y => println("first:"+x)
    case List() => println("empty")
}

val c = List(1,2)
val d = List(2,3)
val e = List(2,5)
StdList.hasSubseq(a,c)
StdList.hasSubseq(a,d)
StdList.hasSubseq(a,e)
StdList.hasSubseq(a,a)

