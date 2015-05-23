package StdList

object StdList {
    def Hello(): Unit = {
        println("hi")
    }
    def hasSubseq[A](sup:List[A], sub:List[A]): Boolean = {
        def go[A](p:List[A], b:List[A], r:List[A]): Boolean = (p, b) match {
            case (_, List()) => {
                //print("final r:"); println(r);
                if (r == sub) true else false
            }
            case (List(), _) => false
            case (h1::t1, h2::t2) => {
                //print("p:"); print(p); print("b:"); print(b); print("r:"); println(r);
                if (h1 == h2) go(t1, t2, r:+h1)
                else go(t1, sub, r)
            }
        }
        go(sup, sub, List())
    }
}
