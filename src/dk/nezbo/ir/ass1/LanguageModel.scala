package dk.nezbo.ir.ass1

import scala.collection.Seq
import ch.ethz.dal.tinyir.processing.XMLDocument
import scala.collection.mutable.HashMap
import scala.collection.mutable.PriorityQueue
import scala.util.Try

class LanguageModel extends RelevanceModel {
  
  val cfs : HashMap[String,Int] = new HashMap[String,Int]

  def process(queries : Seq[Seq[String]], docs : Iterator[XMLDocument]): Seq[Seq[String]] = {
    
    val dfs = docs.map(d => d.name -> Main.getTermFrequencies(d)).toMap
    dfs.foreach(d => d._2.foreach(w => cfs(w._1) = cfs.get(w._1).getOrElse(0) + w._2 ))
    
    val totWc = cfs.values.sum
    val totWd = dfs.mapValues(d => d.values.sum)
    
    Main.debug("Documents: "+dfs.size)
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
	result2
  }
  
  def languageScore(query: Seq[String], dfs: Map[String,Int], numWd: Int, numWc: Int) : Double = {
    val wInD = query.filter(w => dfs.contains(w))
    val lambda = 1.0 / scala.math.log(numWd)
    
    wInD.map(w => scala.math.log(1 + ((1 - lambda) / lambda) * (dfs(w).toDouble / numWd) / (cfs.get(w).getOrElse(0).toDouble / numWc))).sum + scala.math.log(lambda)
  }
}