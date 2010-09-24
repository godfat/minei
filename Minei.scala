
package org.godfat.minei

import org.psmonkey.product.client.mine.vo.GameInfo
import org.psmonkey.product.server.mine.AI_Interface
import org.psmonkey.product.server.mine.MineGM

import scala.collection.immutable.{TreeMap, TreeSet}

class Minei extends AI_Interface{
  override def guess(info: GameInfo, xy: Array[Int]){
    val result = Imp(info.getMap()).pick
    println(Imp(info.getMap()).choices)
    xy.update(0, result._1)
    xy.update(1, result._2)
  }
}

// case study:
// 01110
// 0?x?0
// 01110
//
// 00?1?
// 113x?
// 1x?x?
// 11211

case class Imp(val map_raw: Array[Array[Int]]){
  type Possibility = Double
  type MineSize    = Int

  type Choices = TreeMap[Pos, Possibility]
  type MineMap = TreeMap[Pos, MineSize]

  val  Choices = TreeMap
  val  MineMap = TreeMap

  type Idx = Int
  type Pos = (Idx, Idx)

  case class Clue(val amount: MineSize, val poses: List[Pos])
    extends Ordered[Clue]{
    def possibility: Possibility =
      if(poses.isEmpty){ 0 }else{ amount.toDouble / poses.size }
    // begin horrible! why there's no default lexical comparison?
    def compare(that: Clue) =
      if(compare_amount(that) != 0){ compare_amount(that) }
      else if(poses.size != that.poses.size){
        poses.size.compare(that.poses.size)
      }
      else{
        poses.zip(that.poses).find( (p: (Pos, Pos)) => p._1 != p._2 ) match{
          case Some(p) => Ordering[Pos].compare(p._1, p._2)
          case None    => 0
        }
      }
    def compare_amount(that: Clue) = amount.compare(that.amount)
    //   end horrible! why there's no default lexical comparison?
  }

  case class ClueSet(set: TreeSet[Clue] = TreeSet.empty[Clue]){
    def conclude: Clue = Clue(amount, overlap)
    def amount: Int = if(set.isEmpty){ 0 }else{ set.map(_.amount).min }
    def overlap: List[Pos] =
      if(poses.isEmpty)
        List()
      else
        poses.tail.foldRight(poses.head)(_.intersect(_))
    def poses: List[List[Pos]] = set.map(_.poses).toList
    def +(clue: Clue): ClueSet = ClueSet(set + clue)
  }

  val available: Int = -1
  val mine     : Int = -8

  def width : Int = map_raw.size
  def height: Int = map_raw.head.size

  // pick the most priority
  def pick: Pos =
    choices.max(Ordering[Possibility].on[(_, Possibility)](_._2))._1

  // all choices (available block) with calculated priority
  def choices:      Choices = map_available.foldRight(init_choices)(
    (pos_size: (Pos, MineSize), result: Choices) =>
      result.insert(
        pos_size._1,
        nearby(pos_size._1, map_dug).foldRight(ClueSet())(
          (pos_size: (Pos, MineSize), set: ClueSet) =>
            (set + Clue(pos_size._2 - nearby(pos_size._1, map_mine).size,
                        nearby(pos_size._1, map_available).keys.toList))
        ).conclude.possibility)
  )

  // all choices (available block) with 0 priority
  def init_choices: Choices = TreeMap[Pos, Possibility]()

  // blocks that have already been dug
  def map_dug      : MineMap = map.filter(_._2 >= 1)
  // blocks that we need to examine
  def map_available: MineMap = map.filter(_._2 == available)
  // blocks that contain a mine
  def map_mine     : MineMap = map.filter(_._2 == mine)
  // an empty mine map
  def map_empty    : MineMap = MineMap[Pos, MineSize]()

  // our map data structure
  def map: MineMap =
    Range(0, width).foldRight(map_empty)(
      (x: Idx, m: MineMap) => Range(0, height).foldRight(m)(
        (y: Idx, m: MineMap) => m.insert((x, y), map_raw(x)(y))
      )
    )

  // take nearby blocks
  def nearby(pos: Pos, map: MineMap): MineMap =
    Range(-1, 2).foldRight(map_empty)(
      (x: Idx, result: MineMap) => Range(-1, 2).foldRight(result)(
        (y: Idx, result: MineMap) => {
          val xx = pos._1 + x
          val yy = pos._2 + y
          map.get((xx, yy)) match{
            case Some(size) => result.updated((xx, yy), size) // TODO: insert?
            case _          => result
          }
        }
      )
    )
}

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
