package dk.nezbo.ir.ass1

import scala.collection.Seq
import scala.collection.immutable.Stream
import ch.ethz.dal.tinyir.processing.XMLDocument
import scala.collection.mutable.PriorityQueue
import com.github.aztek.porterstemmer.PorterStemmer

/**
 * A simple model that uses the term frequencies of terms in the queries to
 * judge the relevance of each document.
 */
class TermFrequencyModel extends RelevanceModel {
  
	def process(queries : Seq[Seq[String]], docs : Iterator[XMLDocument]): Seq[Seq[String]] = {
	    var i = 0
	    // ordered in reverse so the worst documents can be skimmed off the top.
	    var topscores = queries.map(q => new PriorityQueue[(String,Double)]()(Ordering.by(Main.ordering))) 
	    
	    var t0 = System.nanoTime()
	    for (doc <- docs) {
	      if(i % 1000 == 0){
	        val t1 = System.nanoTime()
	        Main.debug(i+" files done - "+Main.stemCache.size+" stems in Cache - "+(t1-t0)/1000000000.0+" s")
	        t0 = t1
	      }
	      
	      val tfs = Main.getTermFrequencies(doc)
	      // judge the current document towards each query
	      for(j <- 0 until queries.length){
	        
	        // if it is the worst, it will float to the top and get skimmed off
	        // (like foam on top of a beer glass)
	        topscores(j) += ((doc.name,getTermScore(doc,queries(j),tfs)))
	        if(topscores(j).length > Main.num_to_find)
	          topscores(j).dequeue
	      }
	      
	      i += 1
	    }
	    
	    // retrieving the best documents for each query
	    val result1 = topscores.map(i => i.toList.sortBy(t => -t._2 ))
	    val result2 = result1.map(tt => tt.map(t => t._1))
	    Main.debug(result1)
	    result2
	}
	
	/**
	 * Scoring of the given document based on the term frequencies
	 * and the length of the query.
	 */
	def getTermScore(doc : XMLDocument, qterms : Seq[String], tfs : Map[String,Int]) : Double = {
	    val qtfs = qterms.flatMap(q => tfs.get(q))
		val numTermsInCommon = qtfs.filter(_ > 0).length
		val docEuLen = tfs.values.map(x => x * x).sum.toDouble
		val queryLen = qterms.length.toDouble
		val termOverlap = qtfs.sum / (docEuLen * queryLen)
		numTermsInCommon + termOverlap
	  }
}