
package org.godfat.minei

import org.psmonkey.product.client.mine.vo.GameInfo
import org.psmonkey.product.server.mine.AI_Interface
import org.psmonkey.product.server.mine.MineGM

import scala.collection.immutable.{TreeMap, TreeSet}

class Minei extends AI_Interface{
  override def guess(info: GameInfo, xy: Array[Int]){
    Imp(info.getMap()).debug.fireball match{
      case (x, y) => {
        xy.update(0, x)
        xy.update(1, y)
      }
    }
  }
}

case class Imp(val map_raw: Array[Array[Int]]){
  def debug: Imp = {
    println(choices.filter(_._1 > 0.0))
    return this
  }

  type Possibility = Double
  type MineSize    = Int

  type Choices = List[(Possibility, Pos)]
  type MineMap = TreeMap[Pos, MineSize]

  lazy val  Choices = List
  lazy val  MineMap = TreeMap

  type Idx = Int
  type Pos = (Idx, Idx)

  case class Clue(val amount: MineSize, val poses: List[Pos])
    extends Ordered[Clue]{
    // we want descendant ordering, so use negative numbers
    val possibility: Possibility =
      if(poses.isEmpty) 0 else amount.toDouble / poses.size

    lazy val combos: Int = {
      val n = poses.size
      val k = amount
      factorial(n, n - k + 1) / factorial(k)
    }

    def factorial(i: Int, from: Int = 1): Int = from.to(i).foldRight(1)(_ * _)
    def -(that: Clue) = Clue(amount - that.amount, poses.diff(that.poses))

    // begin horrible! why there's no default lexical comparison?
    def compare(that: Clue) = {
      val amount_compare: Int = amount.compare(that.amount)
      if(amount_compare != 0)
        amount_compare

      else if(poses.size != that.poses.size)
        poses.size.compare(that.poses.size)

      else
        poses.zip(that.poses).find( (p: (Pos, Pos)) => p._1 != p._2 ) match{
          case Some(p) => Ordering[Pos].compare(p._1, p._2)
          case None    => 0
        }
    }
    //   end horrible! why there's no default lexical comparison?
  }

  class DeducedClue(override val possibility: Possibility)
    extends Clue(0, List[Pos]())
  def DeducedClue(possibility: Possibility) = new DeducedClue(possibility)

  // set is used to filter the same clues
  case class ClueSet(pos: Pos, set: TreeSet[Clue] = TreeSet.empty[Clue]){
    def debug: ClueSet = {
      print(pos)
      print(": possibility: ")
      print(combos_hit)
      print(" / ")
      println(combos)
      println(set)
      return this
    }

    lazy val poses: List[List[Pos]] = set.map(_.poses).toList
    lazy val conclude: Clue = compact.conclude_compacted
    // conclude after compact
    lazy val conclude_compacted: Clue =
      if(set.isEmpty)
        DeducedClue(0)
      else
        set.find((c: Clue) => c.amount == c.poses.size) match{
          case Some(c) => c
          case None    => DeducedClue(possibility)
        }

    // remove useless clue
    lazy val compact: ClueSet = ClueSet(pos, set.filter(_.amount > 0))

    // TODO: we haven't considered complex overlap,
    //       say A overlaps with B,
    //           B overlaps with C, and
    //           A didn't overlap with C
    lazy val possibility: Possibility =
      if(combos == 0) 0 else combos_hit.toDouble / (combos_hit + combos_miss)

    // lazy val combos: Int =
    //   min.to(max).foldRight(0)( (size: MineSize, combos: Int) => {
    //     val overlap_clue = Clue(size, overlap)
    //     overlap_clue    .combos * exclusive_combos(overlap_clue) + combos
    //   })
    lazy val combos: Int = combos_hit + combos_miss

    lazy val combos_hit: Int =
      calculate_combos(min_hit, max, 1)
      // min_hit.to(max).foldRight(0)( (size: MineSize, combos: Int) => {
      //   val overlap_clue     = Clue(size, overlap)
      //   val overlap_clue_hit = overlap_clue - Clue(1, List(pos))
      //
      //   overlap_clue_hit.combos * exclusive_combos(overlap_clue) + combos
      // })

    lazy val combos_miss: Int =
      calculate_combos(min, max, 0)
      // min.to(max).foldRight(0)( (size: MineSize, combos: Int) => {
      //   val overlap_clue     = Clue(size, overlap)
      //   val overlap_clue_miss = overlap_clue - Clue(0, List(pos))
      //
      //   overlap_clue_miss.combos * exclusive_combos(overlap_clue) + combos
      // })

    def calculate_combos(min: Int, max: Int, hit: Int): Int =
      min.to(max).foldRight(0)( (size: MineSize, combos: Int) => {
        val overlap_clue = Clue(size, overlap)
        val without_pos  = overlap_clue - Clue(hit, List(pos))
        without_pos.combos * exclusive_combos(overlap_clue) + combos
      })

    lazy val min_hit = List(min, 1).max

    lazy val min: MineSize =
      (set.map((clue) => clue.amount - (clue.poses.size - overlap.size)
      ) +            0).max

    lazy val max: MineSize =
      (set.map((clue) => clue.amount
      ) + overlap.size).min

    lazy val overlap: List[Pos] =
      if(poses.isEmpty) List()
      else              poses.tail.foldRight(poses.head)(_.intersect(_))

    lazy val exclusive_overlap: List[Pos] = overlap.filter(_ != pos)

    def exclusive_combos(overlap_clue: Clue): Int =
      set.foldRight(1)( (clue: Clue, combos: Int) =>
        (clue - overlap_clue).combos * combos)

    def +(clue: Clue): ClueSet = ClueSet(pos, set + clue)
  }

  lazy val available: Int = -1
  lazy val mine     : Int =  9

  lazy val width : Int = map_raw.size
  lazy val height: Int = map_raw.head.size

  // all choices (available block) with 0 priority
  lazy val init_choices: Choices = Choices[(Possibility, Pos)]()

  // blocks that have already been dug
  lazy val map_dug      : MineMap = map.filter(_._2 >= 1)
  // blocks that we need to examine
  lazy val map_available: MineMap = map.filter(_._2 == available)
  // blocks that contain a mine
  lazy val map_mine     : MineMap = map.filter(_._2.abs == mine)
  // an empty mine map
  lazy val map_empty    : MineMap = MineMap[Pos, MineSize]()

  // our map data structure
  lazy val map: MineMap =
    0.until(width).foldRight(map_empty)(
      (x: Idx, m: MineMap) => 0.until(height).foldRight(m)(
        (y: Idx, m: MineMap) => m.insert((x, y), map_raw(x)(y))
      )
    )

  // pick the best result
  lazy val fireball: Pos =
    if(choices50.isEmpty) choices  .last._2
    else                  choices50.head._2

  lazy val choices50: Choices = choices.filter(_._1 >= 0.5)

  // all choices (available block) with calculated priority
  lazy val choices: Choices = map_available.foldRight(init_choices)(
    (pos_size: (Pos, MineSize), result: Choices) => ({
      val pos: Pos = pos_size._1
      val clue_set = nearby(pos, map_dug).foldRight(ClueSet(pos))(
        (pos_size: (Pos, MineSize), set: ClueSet) =>
          (set + Clue(pos_size._2 - nearby(pos_size._1, map_mine).size,
                      nearby(pos_size._1, map_available).keys.toList)))
      (clue_set.debug.conclude.possibility, pos_size._1) :: result
    }).sortBy(-_._1)
  )

  // take nearby blocks
  def nearby(pos: Pos, map: MineMap): MineMap =
    (-1).to(1).foldRight(map_empty)(
      (x: Idx, result: MineMap) => (-1).to(1).foldRight(result)(
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
