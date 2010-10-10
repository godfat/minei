
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

  type Choices = TreeSet[(Probability, Tile)]
  type MineMap = TreeMap[Tile, MineSize]
  type TileSet = TreeSet[Tile]
  type ClueSet = TreeSet[Clue]

  lazy val Choices = TreeSet
  lazy val MineMap = TreeMap
  lazy val TileSet = TreeSet
  lazy val ClueSet = TreeSet

  lazy val emptyMineMap = MineMap[Tile, MineSize]()
  lazy val emptyChoices = Choices[(Probability, Tile)]()
  lazy val emptyTileSet = TileSet[Tile]()
  lazy val emptyClueSet = ClueSet[Clue]()}



trait      AbstractClue{}
case class DefiniteClue() extends AbstractClue

trait Clue extends AbstractClue with Ordered[Clue]{
  val min: T.MineSize
  val max: T.MineSize
  val tiles: T.TileSet

  def --(set: T.ClueSet): Clue = set.foldRight(this)((c, r) => r -- c)

  def --(that: Clue): Clue = {
    val intersected    = tiles & that.tiles
    val exclusive_size = tiles.size - intersected.size
    val min = List(0,
                   this.min - List(intersected.size, that.max  ).min).max
    val max = List(exclusive_size,
                   this.max - List(0, that.min - exclusive_size).max).min
    val left_tiles = tiles -- that.tiles

    if(min == max)  ExclusiveClue(min,      left_tiles)
    else           SubtractedClue(min, max, left_tiles)}


  def &(that: Clue): Clue = {
    val intersected = tiles & that.tiles
    val min = List(0,
                   this.min - (this.tiles.size - intersected.size),
                   that.min - (that.tiles.size - intersected.size)).max
    val max = List(intersected.size, this.max, that.max).min

    if(min == max)  ExclusiveClue(min,      intersected)
    else           ConjunctedClue(min, max, intersected)}


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

  def factorial(i: Int, from: Int = 1): Int = from.to(i).foldRight(1)(_ * _)}



case class ConjunctedClue(val min  : T.MineSize,
                          val max  : T.MineSize,
                          val tiles: T.TileSet) extends Clue

case class SubtractedClue(val min  : T.MineSize,
                          val max  : T.MineSize,
                          val tiles: T.TileSet) extends Clue

// set is used to filter the same clues
case class Conclusion(            tile: T.Tile,
                       exclusive_clues: T.ClueSet,
                      conjuncted_clues: List[T.ClueSet]) extends CountUtil{
  // def debug: Conclusion = {
  //   print(tile)
  //   print(": probability: ")
  //   print(count_hit)
  //   print(" / ")
  //   println(count)
  //   println(exclusive_clues)
  //   return this}

  // conclude after compact
  // lazy val conclude_compacted: AbstractClue =
  //   if(clues.isEmpty)
  //     DefiniteClue(0)
  //   else
  //     clues.find((clue) => clue.size == clue.tiles.size) match{
  //       case Some(clue) => DefiniteClue(1)
  //       case None       => DefiniteClue(probability)}

  lazy val count: Int =
    calculate_count(conjuncted_clues.reverse,
                    T.emptyClueSet + ExclusiveClue(1, T.emptyTileSet + tile))}



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



trait CountUtil{
  val count: Int

  val  exclusive_clues: T.ClueSet
  val conjuncted_clues: List[T.ClueSet]

  protected def calculate_count(    list: List[T.ClueSet],
                              excluded: T.ClueSet = T.emptyClueSet,
                                result: Int = 1): Int =
    list match{
      case Nil             =>
        exclusive_clues.map(_  -- excluded).foldRight(result)(
          (c, r) => c.asInstanceOf[ExclusiveClue].count * r)

      case (clues :: left) =>
        result + split_conjuncted_clues(clues).foldRight(0)((picked, r) =>
          r + calculate_count(
                left.map(T.emptyClueSet ++ _.map(_  -- picked)), picked, r))}

  private def split_conjuncted_clues(clues: T.ClueSet):
                                            List[T.ClueSet] =
    size_combos(clues).map(to_exclusive_clues(clues, _))

  private def size_combos(clues: T.ClueSet): List[List[T.MineSize]] =
    combos(clues.map((clue) => clue.min.to(clue.max).toList).toList)

  private def to_exclusive_clues(clues: T.ClueSet,
                                 sizes: List[T.MineSize]):
                                        T.ClueSet =
    T.emptyClueSet ++ clues.zip(sizes).map((cs) =>
      ExclusiveClue(cs._2, cs._1.tiles))

  private def combos[A](list: List[List[A]]): List[List[A]] = list match{
    case Nil         => List(Nil)
    case (xs :: xss) => for(x <- xs; rs <- combos(xss)) yield x :: rs}}



case class Segment(val map: T.MineMap) extends MapUtil with CountUtil{

  // all choices (available block) with calculated priority
  lazy val choices: T.Choices =
    T.emptyChoices ++ conclusions.map((conclusion) =>
      (conclusion.count.toDouble / count, conclusion.tile))

  lazy val count: Int = calculate_count(conjuncted_clues.reverse)

  lazy val conclusions =
    map_available.keys.map((tile) =>
      Conclusion(tile, exclusive_clues, conjuncted_clues))

  lazy val exclusive_clues: T.ClueSet = map_dug.foldRight(T.emptyClueSet)(
    (tile_size, result) => {
      val tile = tile_size._1
      val size = tile_size._2
      val mines = size - nearby(tile, map_mine).size
      val tiles = T.emptyTileSet ++ nearby(tile, map_available).keys
      result + ExclusiveClue(mines, tiles)})

  lazy val conjuncted_clues: List[T.ClueSet] =
    conjunct_clues(exclusive_clues.toList)

  private def conjunct_clues( clues: List[Clue],
                             result: List[T.ClueSet] = List()):
                                     List[T.ClueSet] = {
    val conjuncted = T.emptyClueSet ++ combos_pair(clues).map(conjunct(_))
    if(conjuncted.isEmpty)
      result
    else
      conjuncted :: conjunct_clues(conjuncted.toList, result)}

  private def conjunct(cc: (Clue, Clue)): Clue = cc._1 & cc._2

  private def combos_pair[A](list: List[A]): List[(A, A)] = list match{
    case Nil       => Nil
    case (x :: xs) => (for(y <- xs) yield (x, y)) ++ combos_pair(xs)}}



case class Imp(val map: T.MineMap) extends MapUtil{
  def debug: Imp = {
    println(choices.filter(_._1 > 0.0))
    return this}

  // pick the best result
  lazy val aim: T.Tile =
    if(choices50.isEmpty) choices  .head._2
    else                  choices50.last._2

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

  lazy val choices: T.Choices = segments.foldRight(T.emptyChoices)(
    (segment, result) => segment.choices ++ result)}



object Imp{
  def create(map: Array[Array[Int]]): Imp = {
    val width : Int = map.size
    val height: Int = map.head.size

    Imp(0.until(width).foldRight(T.emptyMineMap)(
      (x: T.Index, result) => 0.until(height).foldRight(result)(
        (y: T.Index, result) => result.insert((x, y), map(x)(y)))))}}
