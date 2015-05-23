import Tree._
val a = Branch(Leaf(1), Leaf(2))
val b = Branch(Leaf(1), Leaf(2))
val c = Branch(Leaf(2), Leaf(1))
a == b
b == c
val d = Branch(a,c)
val e = Branch(a, Leaf(3))
Tree.size(a)
a match {
    case Branch(l,r) => println(l)
}
Tree.size(e)
Tree.size(d)
3 max 5
3.max(5)
Tree.max(e)
Tree.depth(a)
Tree.depth(d)
Tree.depth(e)
Tree.map(e)(x => x*5)


