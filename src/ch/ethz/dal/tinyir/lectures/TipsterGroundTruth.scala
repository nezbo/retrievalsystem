package ch.ethz.dal.tinyir.lectures

import scala.io.Source
import scala.collection.immutable.Map
import scala.Array.canBuildFrom

class TipsterGroundTruth(path:String) {
  val judgements: Map[Int, Array[String]] =
  Source.fromFile(path).getLines()
  .filter(l => !l.endsWith("0"))
  .map(l => l.split(" "))
  .map(e => (e(0).toInt,e(2).replaceAll("-", "")))
  .toArray
  .groupBy(_._1)
  .mapValues(_.map(_._2))

}

object TipsterGroundTruth {
  
  def main (args:Array[String]){
    val t = new TipsterGroundTruth("tipster/qrels")
    t.judgements.foreach(j => println("Topic "+j._1 +": "+j._2.toList+" judgements found."))
  }
}