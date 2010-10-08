
package org.godfat.minei

import org.psmonkey.product.client.mine.vo.GameInfo
import org.psmonkey.product.server.mine.AI_Interface
import org.psmonkey.product.server.mine.MineGM

import scala.collection.immutable.{TreeMap, TreeSet}

class Minei extends AI_Interface{
  override def guess(info: GameInfo, xy: Array[Int]) = {
    Imp.create(psmonkey(info.getMap())).debug.aim match{
      case (x, y) => {
        xy.update(0, x)
        xy.update(1, y)}}}

  private def psmonkey(map: Array[Array[Int]]): Array[Array[Int]] =
    map.map((ys) => ys.map((v) =>
      if(v.abs == 9) T.mine
      else           v ))}



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

  lazy val emptyMineMap = MineMap[Tile, MineSize]()
  lazy val emptyChoices = Choices[(Probability, Tile)]()
  lazy val emptyTileSet = TileSet[Tile]()
  lazy val emptyClueSet = TreeSet[Clue]()}



trait      AbstractClue{val probability: T.Probability}
case class DefiniteClue(val probability: T.Probability) extends AbstractClue

trait Clue extends AbstractClue with Ordered[Clue]{
  val min: T.MineSize
  val max: T.MineSize
  val tiles: T.TileSet

  def --(that: Clue): SubtractedClue = {
    val intersected    = tiles & that.tiles
    val exclusive_size = tiles.size - intersected.size
    val min = List(0,
                   this.min - List(intersected.size, that.max  ).min).max
    val max = List(exclusive_size,
                   this.max - List(0, that.min - exclusive_size).max).min
    SubtractedClue(min, max, tiles -- that.tiles)}

  def &(that: Clue): ConjunctedClue = {
    val intersected = tiles & that.tiles
    val min = List(0,
                   this.min - (this.tiles.size - intersected.size),
                   that.min - (that.tiles.size - intersected.size)).max
    val max = List(intersected.size, this.max, that.max).min
    ConjunctedClue(min, max, intersected)}

  // begin horrible! why there's no default lexical comparison?
  def compare(that: Clue) = {
    val size_compare: Int =
      Ordering[(T.MineSize, T.MineSize)].compare((this.min, this.max),
                                                 (that.min, that.max))
    if(size_compare != 0)
      size_compare

    else if(tiles.size != that.tiles.size)
      tiles.size.compare(that.tiles.size)

    else
      tiles.zip(that.tiles).find((tt) => tt._1 != tt._2) match{
        case Some(tt) => Ordering[T.Tile].compare(tt._1, tt._2)
        case None     => 0}}}
  //   end horrible! why there's no default lexical comparison?



case class  ExclusiveClue(val  size: T.MineSize,
                          val tiles: T.TileSet) extends Clue{
  val min = size
  val max = size

  lazy val count: Int = {
    val n = tiles.size
    val k = size
    factorial(n, n - k + 1) / factorial(k)}

  def factorial(i: Int, from: Int = 1): Int = from.to(i).foldRight(1)(_ * _)
}

case class ConjunctedClue(val min  : T.MineSize,
                          val max  : T.MineSize,
                          val tiles: T.TileSet) extends Clue

case class SubtractedClue(val min  : T.MineSize,
                          val max  : T.MineSize,
                          val tiles: T.TileSet) extends Clue

// set is used to filter the same clues
case class Conclusion(tile: T.Tile, clues: T.ClueSet = T.emptyClueSet){
  def debug: Conclusion = {
    print(tile)
    print(": probability: ")
    print(count_hit)
    print(" / ")
    println(count)
    println(clues)
    return this}

  lazy val tiles: List[T.TileSet] = clues.map(_.tiles).toList
  lazy val conclude: AbstractClue = compact.conclude_compacted
  // conclude after compact
  lazy val conclude_compacted: AbstractClue =
    if(clues.isEmpty)
      DefiniteClue(0)
    else
      clues.find((clue) => clue.size == clue.tiles.size) match{
        case Some(clue) => DefiniteClue(1)
        case None       => DefiniteClue(probability)}

  // remove useless clue
  lazy val compact: Conclusion = Conclusion(tile, clues.filter(_.size > 0))

  // TODO: we haven't considered complex overlap,
  //       say A overlaps with B,
  //           B overlaps with C, and
  //           A didn't overlap with C
  lazy val probability: T.Probability =
    if(count == 0) 0 else count_hit.toDouble / (count_hit + count_miss)

  lazy val count: Int = count_hit + count_miss

  lazy val count_hit: Int =
    calculate_count(min_hit, max, ExclusiveClue(1, T.TileSet(tile)))

  lazy val count_miss: Int =
    calculate_count(min, max, ExclusiveClue(0, T.TileSet(tile)))

  // same as count, but with only one foldr
  lazy val count_fast: Int =
    calculate_count(min, max, ExclusiveClue(0, T.TileSet()))

  // lazy val combos_all: Int =
  //   overlaps.map((o: Overlap) => o.min.to(o.max).toList).
  //     combinations.foldRight(0)(
  //       (sizes: List[Int], combos: Int) =>
  //         overlap + combos
  //     )

  private def calculate_count(min: Int, max: Int, clue: Clue): Int =
    min.to(max).foldRight(0)((size, count) => {
      val overlap_clue = ExclusiveClue(size, overlap)
      val without_pos  = overlap_clue -- clue
      without_pos.count * exclusive_count(overlap_clue) + count})

  lazy val min_hit = List(min, 1).max

  lazy val min: T.MineSize =
    (clues.map((clue) => clue.size - (clue.tiles.size - overlap.size)
    ) +            0).max

  lazy val max: T.MineSize =
    (clues.map((clue) => clue.size
    ) + overlap.size).min

  lazy val overlap: T.TileSet =
    if(tiles.isEmpty) T.TileSet()
    else              tiles.tail.foldRight(tiles.head)(_.intersect(_))

  // lazy val overlap_list: List[List[T.Pos]] =


  lazy val exclusive_overlap: T.TileSet = overlap - tile

  def exclusive_count(overlap_clue: Clue): Int =
    clues.foldRight(1)((clue, count) => (clue -- overlap_clue).count * count)

  def +(clue: Clue): Conclusion = Conclusion(tile, clues + clue)}



trait MapUtil{
  val map: T.MineMap

  // blocks that have already been dug
  lazy val map_dug      : T.MineMap = map.filter(_._2 >= T.dug)
  // blocks that we need to examine
  lazy val map_available: T.MineMap = map.filter(_._2 == T.available)
  // blocks that contain a mine
  lazy val map_mine     : T.MineMap = map.filter(_._2 == T.mine)

  // take nearby blocks
  def nearby(tile: T.Tile, map: T.MineMap): T.MineMap =
    (-1).to(1).foldRight(T.emptyMineMap)((x, result) =>
      (-1).to(1).foldRight(result)((y, result) => {
        val xx = tile._1 + x
        val yy = tile._2 + y
        map.get((xx, yy)) match{
          case Some(size) => result.updated((xx, yy), size) // TODO: insert?
          case _          => result}}))}



case class Segment(val map: T.MineMap) extends MapUtil{

  lazy val exclusive_clues: T.ClueSet = map_dug.foldRight(T.emptyClueSet)(
    (tile_size, result) => {
      val tile = tile_size._1
      val size = tile_size._2
      val mines = size - nearby(tile, map_mine).size
      val tiles = T.emptyTileSet ++ nearby(tile, map_available).keys
      result + ExclusiveClue(mines, tiles)})

  lazy val conclusions =
    map_available.keys.map((tile) => Conclusion(tile, clues))

  lazy val clues: List[List[Clue]] = process_clues(exclusive_clues.toList,
                                                   List())

  private def process_clues(clues : List[Clue],
                            result: List[List[Clue]]): List[List[Clue]] = {
    val conjuncted = T.emptyClueSet ++ combos_pair(clues).map(conjunct(_))
    if(conjuncted.isEmpty)
      clues :: result
    else
      clues :: process_clues(conjuncted.toList, result)}

  private def combos_pair[A](list: List[A]): List[(A, A)] = list match{
    case Nil       => Nil
    case (x :: xs) => (for(y <- xs) yield (x, y)) ++ combos_pair(xs)}

  private def conjunct(cc: (Clue, Clue)): ConjunctedClue = cc._1 & cc._2
}



case class Imp(val map: T.MineMap) extends MapUtil{
  def debug: Imp = {
    println(choices.filter(_._1 > 0.0))
    return this}


  // pick the best result
  lazy val aim: T.Tile =
    if(choices50.isEmpty) choices  .last._2
    else                  choices50.head._2

  lazy val choices50: T.Choices = choices.filter(_._1 >= 0.5)

  lazy val segments: List[Segment] =
    map_available.foldRight((List[Segment](), T.emptyTileSet))(
      (available_size, segments_tiles) => {
        val available = available_size._1
        val size      = available_size._2
        val segments  = segments_tiles._1
        val tiles     = segments_tiles._2
        if(tiles.contains(available)) // already in some segment
          segments_tiles
        else{
          val segment = Segment(expand_available(available, T.emptyMineMap))
          (segment :: segments, tiles ++ segment.map.keys)
    }})._1

  def expand_available(available: T.Tile, result: T.MineMap): T.MineMap =
    nearby(available, map_dug).foldRight(result)(
      (dug_size, result) =>
        if(result.contains(dug_size._1))
          result
        else
          expand_dug(dug_size._1,
                     result + dug_size) ++ result)

  def expand_dug(dug: T.Tile, result: T.MineMap): T.MineMap =
    nearby(dug, map_available).foldRight(result)(
      (available_size, result) =>
        if(result.contains(available_size._1))
          result
        else
          expand_available(available_size._1,
                           result + available_size) ++ result)

  // all choices (available block) with calculated priority
  lazy val choices: T.Choices = map_available.foldRight(T.emptyChoices)(
    (tile_size, result) => ({
      val tile = tile_size._1
      val conclusion = nearby(tile, map_dug).foldRight(Conclusion(tile))(
        (tile_size, con) => {
          val remaining = tile_size._2 - nearby(tile_size._1, map_mine).size
          val set = T.emptyTileSet ++ nearby(tile_size._1, map_available).keys
          con + ExclusiveClue(remaining, set)})
      (conclusion.debug.conclude.probability, tile_size._1) :: result
    }).sortBy(-_._1)
  )
}



object Imp{
  def create(map: Array[Array[Int]]): Imp = {
    val width : Int = map.size
    val height: Int = map.head.size

    Imp(0.until(width).foldRight(T.emptyMineMap)(
      (x: T.Index, result) => 0.until(height).foldRight(result)(
        (y: T.Index, result) => result.insert((x, y), map(x)(y)))))}}
