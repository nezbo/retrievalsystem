

package ch.ethz.dal.tinyir.io

import ch.ethz.dal.tinyir.processing.XMLDocument
import ch.ethz.dal.tinyir.processing.TipsterParse
import ch.ethz.dal.tinyir.util.StopWatch

class TipsterStream (path: String, ext: String = "") 
extends ParsedXMLStream(new ZipDirStream(path, "")){
  def stream : Stream[XMLDocument] = unparsed.stream.map(is => new TipsterParse(is))
  def length = unparsed.length 
}

object TipsterStream  {

  def main(args: Array[String]) {
    val sw = new StopWatch; sw.start
    val tipster = new TipsterStream ("tipster/zips")  
    println("Number of files in zips = " + tipster.length)
    
    var length : Long = 0 
    var tokens : Long = 0
    for (doc <- tipster.stream.take(10000)) { 
      length += doc.content.length          
      tokens += doc.tokens.length
    }
    sw.stop
    println("Stopped time = " + sw.stopped)
    println("Final number of characters = " + length)
    println("Final number of tokens     = " + tokens)
  }
}