
package org.godfat.minei

import org.psmonkey.product.client.mine.vo.GameInfo
import org.psmonkey.product.server.mine.AI_Interface
import org.psmonkey.product.server.mine.MineGM

import scala.collection.immutable.{TreeMap, TreeSet}

class Minei extends AI_Interface{
  override def guess(info: GameInfo, xy: Array[Int]) = {
    Imp.create(psmonkey(info.getMap())).debug.fireball match{
      case (x, y) => {
        xy.update(0, x)
        xy.update(1, y)
      }
    }
  }

  private def psmonkey(map: Array[Array[Int]]): Array[Array[Int]] =
    map.map((ys) => ys.map((v) =>
      if(v.abs == 9) T.mine
      else           v ))
}

object T{
  lazy val dug      : Int =  1
  lazy val available: Int = -1
  lazy val mine     : Int = -2

  type Probability = Double
  type MineSize    = Int
  type Index       = Int
  type Tile        = (Index, Index)

  type Choices = List[(Probability, Tile)]
  type MineMap = TreeMap[Tile, MineSize]
  type TileSet = TreeSet[Tile]
  type ClueSet = TreeSet[Clue]

  lazy val Choices = List
  lazy val MineMap = TreeMap
  lazy val TileSet = TreeSet
  lazy val ClueSet = TreeSet

  lazy val EmptyMineMap = MineMap[Tile, MineSize]()
  lazy val EmptyChoices = Choices[(Probability, Tile)]()
  lazy val EmptyTileSet = TileSet[Tile]()
  lazy val EmptyClueSet = TreeSet[Clue]()
}

trait AbstractClue{ val probability: T.Probability }

case class DefiniteClue(override val probability: T.Probability)
  extends AbstractClue

case class Clue(val size: T.MineSize, val set: T.TileSet)
  extends AbstractClue with Ordered[Clue]{
  // we want descendant ordering, so use negative numbers
  override lazy val probability: T.Probability =
    if(set.isEmpty) 0 else size.toDouble / set.size

  lazy val count: Int = {
    val n = set.size
    val k = size
    factorial(n, n - k + 1) / factorial(k)
  }

  def factorial(i: Int, from: Int = 1): Int = from.to(i).foldRight(1)(_ * _)
  def --(that: Clue): Clue = if(that.set.subsetOf(set))
                               Clue(size - that.size, set -- that.set)
                             else
                               this

  // begin horrible! why there's no default lexical comparison?
  def compare(that: Clue) = {
    val size_compare: Int = size.compare(that.size)
    if(size_compare != 0)
      size_compare

    else if(set.size != that.set.size)
      set.size.compare(that.set.size)

    else
      set.zip(that.set).find( (p: (T.Tile, T.Tile)) => p._1 != p._2 ) match{
        case Some(p) => Ordering[T.Tile].compare(p._1, p._2)
        case None    => 0
      }
  }
  //   end horrible! why there's no default lexical comparison?
}

// case class ConjunctedClause

// set is used to filter the same clues
case class Conclusion(tile: T.Tile, set: T.ClueSet = T.EmptyClueSet){
  def debug: Conclusion = {
    print(tile)
    print(": probability: ")
    print(count_hit)
    print(" / ")
    println(count)
    println(set)
    return this
  }

  lazy val tiles: List[T.TileSet] = set.map(_.set).toList
  lazy val conclude: AbstractClue = compact.conclude_compacted
  // conclude after compact
  lazy val conclude_compacted: AbstractClue =
    if(set.isEmpty)
      DefiniteClue(0)
    else
      set.find((c: Clue) => c.size == c.set.size) match{
        case Some(c) => c
        case None    => DefiniteClue(probability)
      }

  // remove useless clue
  lazy val compact: Conclusion = Conclusion(tile, set.filter(_.size > 0))

  // TODO: we haven't considered complex overlap,
  //       say A overlaps with B,
  //           B overlaps with C, and
  //           A didn't overlap with C
  lazy val probability: T.Probability =
    if(count == 0) 0 else count_hit.toDouble / (count_hit + count_miss)

  lazy val count: Int = count_hit + count_miss

  lazy val count_hit: Int =
    calculate_count(min_hit, max, Clue(1, T.TileSet(tile)))

  lazy val count_miss: Int =
    calculate_count(min, max, Clue(0, T.TileSet(tile)))

  // same as count, but with only one foldr
  lazy val count_fast: Int =
    calculate_count(min, max, Clue(0, T.TileSet()))

  // lazy val combos_all: Int =
  //   overlaps.map((o: Overlap) => o.min.to(o.max).toList).
  //     combinations.foldRight(0)(
  //       (sizes: List[Int], combos: Int) =>
  //         overlap + combos
  //     )

  private def calculate_count(min: Int, max: Int, clue: Clue): Int =
    min.to(max).foldRight(0)( (size: T.MineSize, count: Int) => {
      val overlap_clue = Clue(size, overlap)
      val without_pos  = overlap_clue -- clue
      without_pos.count * exclusive_count(overlap_clue) + count
    })

  lazy val min_hit = List(min, 1).max

  lazy val min: T.MineSize =
    (set.map((clue) => clue.size - (clue.set.size - overlap.size)
    ) +            0).max

  lazy val max: T.MineSize =
    (set.map((clue) => clue.size
    ) + overlap.size).min

  lazy val overlap: T.TileSet =
    if(tiles.isEmpty) T.TileSet()
    else              tiles.tail.foldRight(tiles.head)(_.intersect(_))

  // lazy val overlap_list: List[List[T.Pos]] =


  lazy val exclusive_overlap: T.TileSet = overlap - tile

  def exclusive_count(overlap_clue: Clue): Int =
    set.foldRight(1)( (clue: Clue, count: Int) =>
      (clue -- overlap_clue).count * count)

  def +(clue: Clue): Conclusion = Conclusion(tile, set + clue)
}

case class Segment(map: T.MineMap){
  // lazy val conclusions: List[Conclusion]
}

case class Imp(val map: T.MineMap){
  def debug: Imp = {
    println(choices.filter(_._1 > 0.0))
    return this
  }

  // blocks that have already been dug
  lazy val map_dug      : T.MineMap = map.filter(_._2 >= T.dug)
  // blocks that we need to examine
  lazy val map_available: T.MineMap = map.filter(_._2 == T.available)
  // blocks that contain a mine
  lazy val map_mine     : T.MineMap = map.filter(_._2 == T.mine)

  // pick the best result
  lazy val fireball: T.Tile =
    if(choices50.isEmpty) choices  .last._2
    else                  choices50.head._2

  lazy val choices50: T.Choices = choices.filter(_._1 >= 0.5)

  lazy val segments: List[Segment] =
    map_available.foldRight((List[Segment](), T.EmptyTileSet))(
      (available_size: (T.Tile       , T.MineSize),
        segments_set : (List[Segment], T.TileSet)) => {
          val available = available_size._1
          val size      = available_size._2
          val segments  =  segments_set ._1
          val set       =  segments_set ._2
          if(set.contains(available)) // already in some segment
            segments_set
          else{
            val segment = Segment(expand_available(available, T.EmptyMineMap))
            (segment :: segments, set ++ segment.map.keys)
          }
        }
    )._1

  def expand_available(available: T.Tile, result: T.MineMap): T.MineMap =
    nearby(available, map_dug).foldRight(result)(
      (dug_size: (T.Tile, T.MineSize), result: T.MineMap) =>
        if(result.contains(dug_size._1))
          result
        else
          expand_dug(dug_size._1,
                     result + dug_size) ++ result
    )

  def expand_dug(dug: T.Tile, result: T.MineMap): T.MineMap =
    nearby(dug, map_available).foldRight(result)(
      (available_size: (T.Tile, T.MineSize), result: T.MineMap) =>
        if(result.contains(available_size._1))
          result
        else
          expand_available(available_size._1,
                           result + available_size) ++ result
    )

  // all choices (available block) with calculated priority
  lazy val choices: T.Choices = map_available.foldRight(T.EmptyChoices)(
    (tile_size: (T.Tile, T.MineSize), result: T.Choices) => ({
      val tile: T.Tile = tile_size._1
      val conclusion = nearby(tile, map_dug).foldRight(Conclusion(tile))(
        (tile_size: (T.Tile, T.MineSize), con: Conclusion) => {
          val remaining = tile_size._2 - nearby(tile_size._1, map_mine).size
          val set = T.EmptyTileSet ++ nearby(tile_size._1, map_available).keys
          con + Clue(remaining, set)
        })
      (conclusion.debug.conclude.probability, tile_size._1) :: result
    }).sortBy(-_._1)
  )

  // take nearby blocks
  def nearby(tile: T.Tile, map: T.MineMap): T.MineMap =
    (-1).to(1).foldRight(T.EmptyMineMap)(
      (x: T.Index, result: T.MineMap) => (-1).to(1).foldRight(result)(
        (y: T.Index, result: T.MineMap) => {
          val xx = tile._1 + x
          val yy = tile._2 + y
          map.get((xx, yy)) match{
            case Some(size) => result.updated((xx, yy), size) // TODO: insert?
            case _          => result
          }
        }
      )
    )
}

object Imp{
  def create(map: Array[Array[Int]]): Imp = {
    val width : Int = map.size
    val height: Int = map.head.size

    Imp(0.until(width).foldRight(T.EmptyMineMap)(
      (x: T.Index, m: T.MineMap) => 0.until(height).foldRight(m)(
        (y: T.Index, m: T.MineMap) => m.insert((x, y), map(x)(y))
      )
    ))
  }
}
