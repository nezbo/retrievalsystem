package dk.nezbo.ir.ass1

import scala.collection.Seq
import ch.ethz.dal.tinyir.processing.XMLDocument
import scala.collection.mutable.HashMap
import scala.collection.mutable.PriorityQueue
import scala.util.Try
import scala.collection.mutable.ListBuffer

/**
 * My implementation of the language model uses the _probability model_
 * where words are judged on their probability of appearing in a given
 * document and in the collection in general.
 */
class LanguageModel extends RelevanceModel {
  
  val cfs : HashMap[String,Int] = new HashMap[String,Int] // collection frequencies for the terms in the queries
  var totWc : Long = 0 // total word count of the collection

  def process(queries : Seq[Seq[String]], docs : Iterator[XMLDocument]): Seq[Seq[String]] = {
    
    // do first processing (calculating document probabilities 
    // for each query term for each document). This is done to
    // avoid both multiple runs through the documents and storing
    // too much data for the documents in memory.
    val intermediate = new ListBuffer[(String,Seq[Seq[(String,Double)]],Int)]
    var i = 0
    var t0 = System.nanoTime()
    for(doc <- docs){
      // Logging
      if(i > 0 && i % 1000 == 0){
        val t1 = System.nanoTime()
        Main.debug(i+" files done - ("+intermediate.size+" saved) - "+Main.stemCache.size+" stems in Cache - "+(t1-t0)/1000000000.0+" s")
        t0 = t1
      }
      
      val tfs = Main.getTermFrequencies(doc)
      val totWd = tfs.values.sum
      this.totWc += totWd // update collection sum
      
      // Store collection word counts for query terms
      tfs.filter(w => queries.exists(q => q.contains(w._1))).foreach(w => cfs(w._1) = cfs.get(w._1).getOrElse(0) + w._2)
      
      val wInD = queries.map(q => q.filter(w => tfs.contains(w)) ) // only terms present in document
      
      // Only save documents that actually contain a word from a query
      if(wInD.map(q => q.size).sum > 0){
        intermediate += ((doc.name, wInD.map(q => q.map(w => ((w, tfs(w).toDouble / totWd )) )), totWd))
      }
      
      i += 1
    }
    
    Main.debug("Documents: "+intermediate.size)
    Main.debug("Words: "+cfs.size)

    // Second part with the general collection info (and then mapping into final resultSet)
    (0 to queries.length-1)
    	.map(q => intermediate.map(i => ((i._1, interToScore(i,q) )) )
    		.sortBy(s => -s._2)
    		.take(Main.num_to_find).map(s => s._1))
  }
  
  /**
   * Given the intermediate calculates finishes the complete score of
   * a document with the collection-wide information (after finishing
   * run through the documents).
   */
  def interToScore(d: (String,Seq[Seq[(String,Double)]],Int), q: Int) : Double = {
    val lambda = 1.0 / d._3 // (document length)
    d._2(q).filter(qu => cfs.contains(qu._1)).map(qu => math.log(1.0 + ((1.0 - lambda)/lambda) * (qu._2 / (cfs(qu._1).toDouble / totWc) ))).sum + math.log(lambda)
  }
}