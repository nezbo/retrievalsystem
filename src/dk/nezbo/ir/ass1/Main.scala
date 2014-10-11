package dk.nezbo.ir.ass1

import ch.ethz.dal.tinyir.io.TipsterStream
import ch.ethz.dal.tinyir.processing.XMLDocument
import scala.collection.mutable.ListBuffer
import com.github.aztek.porterstemmer.PorterStemmer

object Main  {

  def main(args: Array[String]) {
    val t0 = System.nanoTime()
    val tipster = new TipsterStream ("./tipster/zips/")  
    println("Number of files in zips = " + tipster.length)
    
    val t1 = System.nanoTime()
    println("Time elapsed: "+(t1-t0)/1000000000.0+" s")

    var i = 0
    val words = ListBuffer.empty[String] // the words at their location
    val dfs = ListBuffer.empty[(Int,List[Int])] //List[(Int,List[Int])]
    for (doc <- tipster.stream.take(100)) { 
      if(i % 1000 == 0) println(i+" files done.")
      
      // DO THINGS HERE
      dfs += ((doc.ID,getWordFrequencies(doc,words)))
      
      i += 1
    }
    val t2 = System.nanoTime()
    println(words)
    println(dfs)
    println("\nTime elapsed: "+(t2-t0)/1000000000.0+" s")
  }
  
  def getWordFrequencies(doc : XMLDocument, old_words : ListBuffer[String]) : List[Int] = {
    val result = ListBuffer.empty[Int]
    val frequencies = doc.tokens.groupBy(identity).map(kv => (kv._1,kv._2.length))
    
    // handle old words
    for(word <- old_words){
       if(frequencies.contains(word)) {
         result += frequencies(word)
       } else {
         result += 0
       }
    }
    
    // new ones
    for(kv <- frequencies.filter(kv => !old_words.contains(kv._1))) {
      old_words += kv._1
      result += kv._2
    }
    
    result.toList
  }
}