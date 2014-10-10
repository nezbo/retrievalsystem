package dk.nezbo.ir.ass1

import ch.ethz.dal.tinyir.io.TipsterStream

object Main  {

  def main(args: Array[String]) {
    val t0 = System.nanoTime()
    val tipster = new TipsterStream ("./tipster/zips/")  
    println("Number of files in zips = " + tipster.length)
    val t1 = System.nanoTime()
    println("Time elapsed: "+(t1-t0)/1000000000.0+" s")
    var length : Long = 0 
    var tokens : Long = 0
    var i = 0
    for (doc <- tipster.stream.take(10000)) { 
      if(i % 1000 == 0) println(i+" files done.")
      length += doc.content.length          
      tokens += doc.tokens.length
      
      i += 1
    }
    val t2 = System.nanoTime()
    println("Time elapsed: "+(t2-t0)/1000000000.0+" s")
    println("Final number of characters = " + length)
    println("Final number of tokens     = " + tokens)
  }
}