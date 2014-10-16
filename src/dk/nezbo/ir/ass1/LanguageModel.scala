package dk.nezbo.ir.ass1

import scala.collection.Seq
import ch.ethz.dal.tinyir.processing.XMLDocument
import scala.collection.mutable.HashMap
import scala.collection.mutable.PriorityQueue
import scala.util.Try
import scala.collection.mutable.ListBuffer

class LanguageModel extends RelevanceModel {
  
  val cfs : HashMap[String,Int] = new HashMap[String,Int]
  var totWc : Long = 0

  def process(queries : Seq[Seq[String]], docs : Iterator[XMLDocument]): Seq[Seq[String]] = {
    
    // do first processing
    val intermediate = new ListBuffer[(String,Seq[Seq[(String,Double)]],Int)]
    var i = 0
    var t0 = System.nanoTime()
    for(doc <- docs){
      // logging
      if(i > 0 && i % 1000 == 0){
        val t1 = System.nanoTime()
        Main.debug(i+" files done - ("+intermediate.size+" saved) - "+Main.stemCache.size+" stems in Cache - "+(t1-t0)/1000000000.0+" s")
        t0 = t1
      }
      
      val tfs = Main.getTermFrequencies(doc)
      val totWd = tfs.values.sum
      this.totWc += totWd // update collection sum
      
      // store collection word counts (only add if more than one occurence)
      tfs.filter(w => queries.exists(q => q.contains(w._1))).foreach(w => cfs(w._1) = cfs.get(w._1).getOrElse(0) + w._2)
      
      // TODO FUCK THIS
      val wInD = queries.map(q => q.filter(w => tfs.contains(w)) )
      
      // only save documents that actually contain a word from a query
      if(wInD.map(q => q.size).sum > 0){
        intermediate += ((doc.name, wInD.map(q => q.map(w => ((w, tfs(w).toDouble / totWd )) )), totWd))
      }
      
      i += 1
    }
    
    Main.debug("Documents: "+intermediate.size)
    Main.debug("Words: "+cfs.size)

    // second part with catalog data (and then mapping into final resultSet
    (0 to queries.length-1)
    	.map(q => intermediate.map(i => ((i._1, interToScore(i,q) )) )
    		.sortBy(s => -s._2)
    		.take(Main.num_to_find).map(s => s._1))
  }
  
  def interToScore(d: (String,Seq[Seq[(String,Double)]],Int), q: Int) : Double = {
    val lambda = 1.0 / d._3
    d._2(q).filter(q => cfs.contains(q._1)).map(q => math.log(1.0 + ((1.0 - lambda)/lambda) * (q._2 / (cfs(q._1).toDouble / totWc) ))).sum + math.log(lambda)
  }
}