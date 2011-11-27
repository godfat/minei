// case study:
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

def create_map(s: String) =
  s.split('\n').map(_.split(',').filter(_ != "").map(_.toInt)).tail.transpose

val map0 = // Tile (1,0): 1 / 1
create_map("""
-1,-1,-1
01,02,01
""")

val map1 = // Tile (0,0): 1 / 1
create_map("""
-1,-1,00
01,02,01
00,01,-9
""")

val map2 = // Tile (1,1): 1 / 1
create_map("""
01,01,01
-1,-1,-1
01,01,01
""")

val map3 =
create_map("""
01,01,01,01,01,01,01
-1,-1,-1,-1,-1,-1,-1
""")

// 01,01,01,01,01,01,01
// xx,01,01,xx,01,01,xx

val map4 = // Tile (1,2): 1 / 1
           // Tile (3,1): 1 / 1
           // Tile (3,2): 1 / 1
create_map("""
00,00,-1,01
01,01,03,-1
01,-1,-1,-1
01,01,02,01
""")

// --Failed, need to re-create segments when any 100% encountered--

// Imp.create(map0).debug
// Imp.create(map1).debug
// Imp.create(map2).debug
Imp.create(map3).debug
// Imp.create(map4).debug
