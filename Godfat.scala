
// package org.godfat.mine

import org.psmonkey.product.client.mine.vo.GameInfo
import org.psmonkey.product.server.mine.AI_Interface
import org.psmonkey.product.server.mine.MineGM

import scala.collection.immutable.TreeMap

class Godfat extends AI_Interface{
  override def guess(info: GameInfo, xy: Array[Int]){
    val result = Imp(info.getMap()).pick
    xy.update(0, result._1)
    xy.update(1, result._2)
  }
}

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

  def width : Int = map_raw.size
  def height: Int = map_raw.head.size

  // pick the most priority
  def pick: Pos = choices.max(Ordering[Priority].on[(Pos, Priority)](_._2))._1

  // all choices (available block) with calculated priority
  def choices:      Choices = map_dug.foldRight(init_choices)(think(_, _))

  // all choices (available block) with 0 priority
  def init_choices: Choices = map.filter(_._2 == available).mapValues(_ => 0).
    asInstanceOf[Choices]

  // blocks that have already been dug
  def map_dug: MineMap = map.filter(_._2 > 0)

  // our map data structure
  def map: MineMap =
    Range(0, width).foldRight(MineMap[Pos, MineSize]())(
      (x: Idx, m: MineMap) => Range(0, height).foldRight(m)(
        (y: Idx, m: MineMap) => m.insert((x, y), map_raw(x)(y))
      )
    )

  // calculate priorities with a dug block
  def think(pos_size: (Pos, MineSize), result: Choices): Choices =
    Range(-1, 1).foldRight(result)(
      (x: Int, result: Choices) => Range(-1, 1).foldRight(result)(
        (y: Int, result: Choices) => nearby()
      )
    )
    nearby(pos_size._1).
    result.insert()

  // take nearby blocks
  def nearby(pos: Pos): List[Pos]
}

println(Imp(Array(Array(0,1), Array(2,3))).map)
