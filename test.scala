// case study:
// 01110
// 0?x?0
// 01110
//
// 00?1?
// 113x?
// 1x?x?
// 11211

// --00-
// x-?3-
// x4?xx
// x-x--

// 2???
// 22??
// -1x?
// 11??

// val imp = Imp(Array(Array(0,1), Array(2,3)))
// println(imp.map)
// val c0 = imp.Clue(1, List((0,0)))
// val c1 = imp.Clue(2, List((0,0)))
// val c2 = imp.Clue(2, List((0,1)))
// val c3 = imp.Clue(2, List((1,0)))
// println(c0.compare(c0)) //  0
// println(c0.compare(c1)) // -1
// println(c0.compare(c2)) // -1
// println(c2.compare(c1)) //  1
// println(c1.compare(c2)) // -1
// println(c2.compare(c3)) // -1

import org.godfat.minei.Imp

val map =
"""
-1,-1,-1,00
00,02,01,00
""".split('\n').map(_.split(',').filter(_ != "").map(_.toInt)).tail

println(Imp.create(map).debug.segments)

def combos[A](list: List[List[A]]): List[List[A]] = list match{
  case Nil         => List[List[A]](List[A]())
  case (xs :: xss) =>  for(x <- xs; rs <- combos(xss)) yield x :: rs
}

def combo2[A](list: List[A]): List[(A, A)] = list match{
  case Nil       => Nil
  case (x :: xs) => (for(y <- xs) yield (x, y)) ++ combo2(xs)
}

println(combos(List(List(1,2), List(3,4), List(5,6))))
println(combo2(List(1,2,3,4,5,6,7,8)))
