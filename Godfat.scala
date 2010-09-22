
package org.godfat.mine

import org.psmonkey.product.client.mine.vo.GameInfo
import org.psmonkey.product.server.mine.AI_Interface
import org.psmonkey.product.server.mine.MineGM

class Godfat extends AI_Interface{
  override def guess(info: GameInfo, xy: Array[Int]){
    val result = think(info.getMap())
    xy.update(0, result._1)
    xy.update(1, result._2)
  }

  def think(map: Array[Array[Int]]): (Int, Int) = (0, 0)
}
