package dk.nezbo.ir.ass1

import scala.collection.Seq
import ch.ethz.dal.tinyir.processing.XMLDocument
import scala.collection.mutable.HashMap
import scala.collection.mutable.PriorityQueue
import scala.util.Try
import scala.collection.mutable.ListBuffer

class LanguageModel extends RelevanceModel {
  
  val cfs : HashMap[String,Int] = new HashMap[String,Int]

  def process(queries : Seq[Seq[String]], docs : Stream[XMLDocument]): Seq[Seq[String]] = {
    
    // do first processing
    val intermediate = new ListBuffer[(String,List[List[Double]],Int)]
    var i = 0
    var t0 = System.nanoTime()
    for(doc <- docs){
      if(i % 1000 == 0){
        Main.debug(i+" files done - "+cfs.size+" words - "+(System.nanoTime()-t0)/1000000000.0+" s")
        t0 = System.nanoTime()
      }
      
      val dfs = Main.getTermFrequencies(doc)
      val totWd = dfs.values.sum
      dfs.foreach(w => cfs(w._1) = cfs.get(w._1).getOrElse(0) + w._2)
      
      val wInD = queries.map(q =>q.filter(w => dfs.contains(w)))
      
      intermediate += ((doc.name, wInD.map(q => q.map(w => dfs(w).toDouble / totWd).toList).toList, totWd))
      i += 1
    }
    
    Main.debug("Documents: "+intermediate.size)
    Main.debug("Words: "+cfs.size)

    // second part with catalog data
    val totWc = cfs.values.sum
    val result = (0 to queries.length-1).map(q => intermediate.map(i => interToScore(i,q,queries(q),totWc)).sortBy(s => -s._2).take(Main.num_to_find).map(s => s._1))
    
    /*Main.debug("Documents: "+dfs.size)
    Main.debug("Words: "+cfs.size)
    
    var i = 0
    var lastQ = 0
    var topscores = queries.map(q => new PriorityQueue[(String,Double)]()(Ordering.by(Main.ordering)))
    for(q <- 0 to queries.length-1; doc <- dfs){
      if(lastQ != 0 || i % 1000 == 0){
        Main.debug("Query "+q+": "+i+" files done.")
        lastQ = q
      }
      
      topscores(q) += ((doc._1,languageScore(queries(q),doc._2,totWd(doc._1),totWc)))
	  if(topscores(q).length > Main.num_to_find) { topscores(q).dequeue }
	  
      i += 1
    }
    
	val result1 = topscores.map(i => i.toList.sortBy(t => -t._2 ))
	val result2 = result1.map(tt => tt.map(t => t._1))
	Main.debug(result1)
	result2*/
    result
  }
  
  def interToScore(d: (String,List[List[Double]],Int), q: Int, qObj: Seq[String], totWc: Int) : (String,Double) = {
    val lambda = scala.math.log(d._3)
    (d._1 , d._2(q).zipWithIndex.map(z => scala.math.log(1.0 + ((1.0 - lambda)/lambda) * (z._1 / cfs(qObj(z._2)) ))).sum + scala.math.log(lambda))
  }
  
  /*def languageScore(query: Seq[String], dfs: Map[String,Int], numWd: Int, numWc: Int) : Double = {
    val wInD = query.filter(w => dfs.contains(w))
    val lambda = 1.0 / scala.math.log(numWd)
    
    wInD.map(w => scala.math.log(1 + ((1 - lambda) / lambda) * (dfs(w).toDouble / numWd) / (cfs.get(w).getOrElse(0).toDouble / numWc))).sum + scala.math.log(lambda)
  }*/
}