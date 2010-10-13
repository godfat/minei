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

/*
segment properties:

  val imp: Imp
  // 0
  imp.segments.foldRight1(_.map ++ _.map) == imp.map
  // 1
  imp.segments.foldRight1(_.mines + _.mines) == imp.mines

clue properties:

  // 0
  clue1 -- clue0 implies clue0 <= clue1
  clue0 <= clue1 implies clue0.min <= clue1.min and
                         clue0.max <= clue1.max and
*/

import org.godfat.minei.Imp

val map =
"""
-1,-1,-1,00
00,02,01,00
""".split('\n').map(_.split(',').filter(_ != "").map(_.toInt)).tail.transpose

val imp = Imp.create(map)
println(imp.segments.map(_.count))
imp.debug
