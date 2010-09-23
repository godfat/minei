
// package org.godfat.minei

import org.psmonkey.product.client.mine.vo.GameInfo
import org.psmonkey.product.server.mine.AI_Interface
import org.psmonkey.product.server.mine.MineGM

import scala.collection.immutable.TreeMap

class Minei extends AI_Interface{
  override def guess(info: GameInfo, xy: Array[Int]){
    val result = Imp(info.getMap()).pick
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
  type Priority = Double
  type MineSize = Int

  type Choices  = TreeMap[Pos, Priority]
  type MineMap  = TreeMap[Pos, MineSize]

  val  Choices = TreeMap
  val  MineMap = TreeMap

  type Idx = Int
  type Pos = (Idx, Idx)

  val available: Int = -1
  val mine     : Int = -8

  def width : Int = map_raw.size
  def height: Int = map_raw.head.size

  // pick the most priority
  def pick: Pos = choices.max(Ordering[Priority].on[(Pos, Priority)](_._2))._1

  // all choices (available block) with calculated priority
  def choices:      Choices = map_available.foldRight(init_choices)(
    (pos_size: (Pos, MineSize), result: Choices) =>
      ClueSet(nearby(pos_size._1, map_dug).map( (pos_size: (Pos, MineSize)) =>
        Clue(pos_size._2, nearby(pos_size._1, map_available).keys.toList)
      ).toList).test(result)
  )

  // all choices (available block) with 0 priority
  def init_choices: Choices = map_available.mapValues(_ => 0).
    asInstanceOf[Choices]

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
    Range(-1, 1).foldRight(map_empty)(
      (x: Int, result: MineMap) => Range(-1, 1).foldRight(result)(
        (y: Int, result: MineMap) =>
          map.get((x, y)) match{
            case Some(size: MineSize) => result.insert((x, y), size)
            case _                    => result
          }
      )
    )

  case class Clue(val amount: MineSize, val poses: List[Pos]){

  }

  case class ClueSet(val clues: List[Clue]){
    def test(result: Choices): Choices = result
  }
}

println(Imp(Array(Array(0,1), Array(2,3))).map)
