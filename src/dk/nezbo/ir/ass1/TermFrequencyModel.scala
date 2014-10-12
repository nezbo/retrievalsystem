package dk.nezbo.ir.ass1

import scala.collection.Seq
import scala.collection.immutable.Stream
import ch.ethz.dal.tinyir.processing.XMLDocument
import scala.collection.mutable.PriorityQueue
import com.github.aztek.porterstemmer.PorterStemmer

class TermFrequencyModel extends RelevanceModel {
	def process(queries : Seq[Seq[String]], docs : Stream[XMLDocument]): Seq[Seq[String]] = {
	    var i = 0
	    var topscores = queries.map(q => new PriorityQueue[(String,Double)]()(Ordering.by(ordering)))
	    
	    for (doc <- docs) {
	      if(i % 1000 == 0) Main.debug(i+" files done.")
	      
	      // DO THINGS HERE
	      for(j <- 0 until queries.length){
	        
	        topscores(j).enqueue((doc.name,getTermScore(doc,queries(j))))
	        if(topscores(j).length > Main.num_to_find)
	          topscores(j).dequeue
	      }
	      
	      i += 1
	    }
	    
	    topscores.foreach(t => Main.debug(t.reverse))
	    topscores.map(i => i.reverse.toList.map(tt => tt._1))
	}
	
	def getTermFrequencies(doc : XMLDocument) : Map[String,Int] = {
	    doc.tokens.map(PorterStemmer.stem(_)).groupBy(identity).mapValues(v => v.length)
	}
	
	def getTermScore(doc : XMLDocument, qterms : Seq[String]) : Double = {
	    val tfs = getTermFrequencies(doc)
	    
	    val qtfs = qterms.flatMap(q => tfs.get(q))
		val numTermsInCommon = qtfs.filter(_ > 0).length
		val docEuLen = tfs.values.map(x => x * x).sum.toDouble
		val queryLen = qterms.length.toDouble
		val termOverlap = qtfs.sum / (docEuLen * queryLen)
		numTermsInCommon + termOverlap
	  }
	
	def ordering(row : (String,Double)) = -row._2
}