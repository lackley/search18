package search
import scala.xml.Node
import scala.util.matching.Regex
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;


//Index object builds index files from a corpus
object Index {
   def main(args: Array[String]) {
      if(args.length != 4){
        println("Invalid arguments given to indexer")
      } else {
        val indexer: Index = new Index(args(0), args(1), args(2), args(3))
        val parsed: (Boolean, List[(Int, Set[String])]) = indexer.parseXML()
        val parsedBool: Boolean = parsed._1
        val pageOutLinks: List[(Int, Set[String])] = parsed._2
        if(parsedBool){
           indexer.addKnightRank(pageOutLinks)
           indexer.writeDocAndTitleFile()
           indexer.writeWordFile()
        }
      }
  
   }  
   
   /**
    * most basic test of test.xml file 
    */
   def test() {
     
      val corpus: String = "/gpfs/main/home/lackley/course/cs018/" +
                            "workspace/scalaproject/src/search/test.xml"
      val doc: String = "/gpfs/main/home/lackley/course/cs018/"+ 
                            "workspace/scalaproject/src/search/docFile.txt"
      val word: String = "/gpfs/main/home/lackley/course/cs018/" + 
                          "workspace/scalaproject/src/search/wordFile.txt"
      val title: String = "/gpfs/main/home/lackley/course/cs018/" + 
                          "workspace/scalaproject/src/search/titleFile.txt"
      val testIndexer = new Index(corpus, title, word, doc)
      testIndexer.parseXML()
      testIndexer.writeDocAndTitleFile()
      testIndexer.writeWordFile()
      println(testIndexer.wordTable.get("a").get._1 == 5)
      println(testIndexer.docInfo.get(0).get._1 == "Page0")
      
   }
   
      /**
    * most basic test of test.xml file 
    */
   def test4() {
     
      val corpus: String = "/gpfs/main/home/lackley/course/cs018/" +
                            "workspace/scalaproject/src/search/test4.xml"
      val doc: String = "/gpfs/main/home/lackley/course/cs018/"+ 
                            "workspace/scalaproject/src/search/docFile4.txt"
      val word: String = "/gpfs/main/home/lackley/course/cs018/" + 
                          "workspace/scalaproject/src/search/wordFile4.txt"
      val title: String = "/gpfs/main/home/lackley/course/cs018/" + 
                          "workspace/scalaproject/src/search/titleFile4.txt"
      val testIndexer = new Index(corpus, title, word, doc)
      testIndexer.parseXML()
      testIndexer.writeDocAndTitleFile()
      testIndexer.writeWordFile()
      
   }
   
   /**
    * tests the smallWiki corpus
    */
   def testSmall() {
      println("parsing small corpus...")
      val corpus: String = "/gpfs/main/home/lackley/course/cs018/" +
                            "workspace/scalaproject/src/search/smallWiki"
      val doc: String = "/gpfs/main/home/lackley/course/cs018/"+ 
                        "workspace/scalaproject/src/search/docFileSmall.txt"
      val word: String = "/gpfs/main/home/lackley/course/cs018/" + 
                         "workspace/scalaproject/src/search/wordFileSmall2.txt"
      val title: String = "/gpfs/main/home/lackley/course/cs018/" + 
                         "workspace/scalaproject/src/search/titleFileSmall.txt"
      val testIndexer = new Index(corpus, title, word, doc)
      testIndexer.parseXML()
      testIndexer.writeDocAndTitleFile()
      testIndexer.writeWordFile()
      println("parsed small corpus")
      
      //test that knight rank vector adds to about 1
      val iter = testIndexer.docInfo.iterator
      var sum: Double = 0
      while(iter.hasNext){
        val next: (Int, (String, Double, Double)) = iter.next
        val kRank: Double = next._2._2
        sum += kRank
      }
      println(sum)
   }
   
    /**
    * tests the bigWiki corpus
    */
   def testBig() {
      println("parsing big corpus...")
      val corpus: String = "/gpfs/main/home/lackley/course/cs018/" +
                            "workspace/scalaproject/src/search/BigWiki"
      val doc: String = "/gpfs/main/home/lackley/course/cs018/"+ 
                        "workspace/scalaproject/src/search/docFileBig.txt"
      val word: String = "/gpfs/main/home/lackley/course/cs018/" + 
                         "workspace/scalaproject/src/search/wordFileBig.txt"
      val title: String = "/gpfs/main/home/lackley/course/cs018/" + 
                         "workspace/scalaproject/src/search/titleFileBig.txt"
      val testIndexer = new Index(corpus, title, word, doc)
      testIndexer.parseXML()
      testIndexer.writeDocAndTitleFile()
      testIndexer.writeWordFile()
      println("parsed big corpus")
      
      
   }
  
   def testMed(){
     println("parsing medium corpus...")
      val corpus: String = "/gpfs/main/home/lackley/course/cs018/" +
                            "workspace/scalaproject/src/search/MedWiki"
      val doc: String = "/gpfs/main/home/lackley/course/cs018/"+ 
                        "workspace/scalaproject/src/search/docFileMed.txt"
      val word: String = "/gpfs/main/home/lackley/course/cs018/" + 
                         "workspace/scalaproject/src/search/wordFileMed.txt"
      val title: String = "/gpfs/main/home/lackley/course/cs018/" + 
                         "workspace/scalaproject/src/search/titleFileMed.txt"
      val testIndexer = new Index(corpus, title, word, doc)
      testIndexer.parseXML()
      testIndexer.writeDocAndTitleFile()
      testIndexer.writeWordFile()
      println("parsed medium corpus")
      //test that knight rank vector adds to about 1
      val iter = testIndexer.docInfo.iterator
      var sum: Double = 0
      while(iter.hasNext){
        val next: (Int, (String, Double, Double)) = iter.next
        val kRank: Double = next._2._2
        sum += kRank
      }
      println(sum)
   }
   
}

/**
 * represents a class for indexing a wikiFile
 * @param wikiFile - a corpus of files to parse
 * @param titleFile - a file to write titles and ids to
 * @param wordFile - a file to write information about every word in corpus
 * @param docFile - a file to write information about docs to
 */
class Index(wikiFile: String, titleFile: String, 
            wordFile: String, docFile: String) {
  
    //word table is a hashtable, the keys are every word in corpus
    //the value is a tuple containg the word frequency in entire corpus
    //and a list that contains the frequency of the word in all documents it appears
    //calling list(docId) should give the number of times the word appears in that doc
    var wordTable: Map[String, (Int, Map[Int, Int])] = Map()
    
    //doc info is a map that for each document contains the name, a map of words and frequencies,
    //knightRank, euclidian norm
    var docInfo: Map[Int, (String, Double, Double)] = Map()
    
    //for links
    var titleToId: Map[String, Int] = Map()
   
/**
 * parses the xml
 * @return true if the xml was parsed false otherwise
 */
  def parseXML(): (Boolean, List[(Int, Set[String])]) = {
    
    var valid: Boolean = true
    var node: Node = null
    try {
      node = xml.XML.loadFile(wikiFile)
    } catch {
      case e: Exception => {
        println("Could not find the wiki file named " + wikiFile)
        valid = false
      }
    }
    var pageOutLinks: List[(Int, Set[String])] = List()
    
    if(valid){
      val childern: Seq[Node] = node.child
      val pages: Seq[Node] = node \ "page"
      
      for(page <- pages){
        var myWords: Map[String, Int] = Map()
        val idStr: String = (page \"id").text.trim()
        val idNum: Int = idStr.toInt
        
        val name: String = (page \"title").text.trim() 
      
        titleToId = titleToId.+((name, idNum))

        val body: String = (page \"text").text
      
        val regex = new Regex("""\[\[[^\[]+?\]\]|[^\W\_]+'[^\W\_]+|[^\W\_]+""")
        val matchIterator = regex.findAllMatchIn(body)
        val strList = matchIterator.toList.map { aMatch => aMatch.matched }
      
        var outLinks: Set[String] = Set()
      
        for(st <- strList){
        
          if(st.startsWith("[[")){
            var link: String = st.substring(2, st.length-2)
            outLinks = outLinks.+(link)
            var title: String = st.substring(2, st.length()-2)
            val regex = new Regex("^[a-zA-Z0-9_]*$")
            val matchIterator = regex.findAllMatchIn(title)
            val titleStrings = matchIterator.toList.map { aMatch => aMatch.matched }

            for(part <- titleStrings){
              var str: String = part.toLowerCase()
              str = PorterStemmer.stem(str)
              addToWordTable(str, idNum)
              val myWordOccur: Option[Int] = myWords.get(str)
              if(myWordOccur != None){
                myWords = myWords.+((str, myWordOccur.get+1))
              } else {
                myWords = myWords.+((str, 1))
              }
            }
          } else {
            var str: String = st.toLowerCase()
            str = PorterStemmer.stem(str)
            addToWordTable(str, idNum)
            val myWordOccur: Option[Int] = myWords.get(str)
            if(myWordOccur != None){
                myWords = myWords.+((str, myWordOccur.get+1))
            } else {
                myWords = myWords.+((str, 1))
            }
          }
        }
        pageOutLinks = (idNum, outLinks) :: pageOutLinks
      
        var euc: Double = 0
        var knightRank: Double = 0

        //loop thru all the words to get euc
        val iter = myWords.valuesIterator
        while(iter.hasNext){
           val next = iter.next
           euc += next * next
        }
        euc = Math.sqrt(euc)
      
        docInfo = docInfo.+((idNum, (name, knightRank, euc)))
      }
      
      
     }
     (valid, pageOutLinks)
    
  }
    
     /**
     * adds a word to wordResult table
     * @param str- word to add
     * @param idNum - id of doc to add it to
     */
    private def addToWordTable(str: String, idNum: Int){
      //if the word is already in the word table 
          //if the doc is already in the list of documents
              //add 1 to the frequency of the doc 
          //if the doc is not already in the list of documents
              //increment dfw
              //add the doc to the list with frequency 1
      //if the word is not in the table
          //add the word to the table with dfw 1
          //add the current document to the list of docs with freq 1
      val wordResult: Option[(Int, Map[Int, Int])] = wordTable.get(str)
      if(wordResult != None){
        //if the word is already in the table
        var docList: Map[Int, Int] = wordResult.get._2
        var dfw: Int = wordResult.get._1
        val docFreqResult: Option[Int] = docList.get(idNum)
        if(docFreqResult != None){
          //if the doc is already in the list of documents
            //add 1 to the frequency of the doc, dont change dfw
          val incrDocFreq = docFreqResult.get + 1
          docList = docList.+((idNum, incrDocFreq))
          wordTable = wordTable.+((str, (dfw, docList)))
        } else {
          //if the doc is not already in list of documents
             //increment dfw
              //add the doc to the list with frequency 1
          val incrDfw = dfw+1
          docList = docList.+((idNum, 1))
          wordTable = wordTable.+((str, (incrDfw, docList)))
        }
      } else {
        //if the word is not in the table
        var docList: Map[Int, Int] = Map()
        docList = docList.+((idNum, 1))
        wordTable = wordTable.+((str, (1, docList)))
      }
      
              
    }
    

    
    /**
     * updates the docInfo hashtable so that it also includes knightRank
     * @param pageOutLinks- a list of tuples containing ids and
     *                      a set of strings that are the doc's out links
     */
    private def addKnightRank(pageOutLinks: List[(Int, Set[String])]) {
      val ranker: KnightRanker = new KnightRanker(.001, titleToId, pageOutLinks)
      val rankings: Array[Double] = ranker.getKnightRanks()
      
      //update docInfo so that it has correct knightRank 
      val iter = docInfo.iterator
      
      while(iter.hasNext) {
        val next: (Int, (String, Double, Double)) = iter.next
        val id: Int = next._1
        val name:  String = next._2._1
        val Ed: Double = next._2._3
        val knightRank: Double = rankings(id)
        
        docInfo = docInfo.+((id, (name, knightRank, Ed)))
      }
      
      
    }

    
     /**
     * writes the word file
     */
    def writeWordFile() {
      var wFile: File = new File(wordFile)
      var writer: BufferedWriter = new BufferedWriter(new FileWriter(wFile.getAbsolutePath()))
     
      val iter = wordTable.iterator
      
      while(iter.hasNext){
        val next: (String, (Int, Map[Int, Int])) = iter.next
        
        val word: String = next._1
        
        val pairValue: (Int, Map[Int, Int]) = next._2
        val dfw: Int = pairValue._1
        val idfw: Double = Math.log10(docInfo.size/(1+dfw.toDouble))
        val docIdFreq: Map[Int, Int] = pairValue._2
        val docIter = docIdFreq.iterator
        
        writer.write("\"" + next._1 + "\" " + idfw + " : ")
        
        while(docIter.hasNext){
          val next: (Int, Int) = docIter.next
          val docId: Int = next._1
          val freq: Int = next._2
          writer.write(docId.toString() + " " + freq.toString() + " ")
        }
        writer.write("\n")
        
      }
      
      writer.close()
      
    }
    

    
    /**
     * writes the doc and title files
     */
    def writeDocAndTitleFile() {
      var dFile: File = new File(docFile)
      var writer: BufferedWriter = new BufferedWriter(new FileWriter(dFile.getAbsolutePath()))
      
      var tFile: File = new File(titleFile)
      var tWriter: BufferedWriter = new BufferedWriter(new FileWriter(tFile.getAbsolutePath()))
      
      val iter = docInfo.iterator
      
      while(iter.hasNext){
        val next: (Int, (String, Double, Double)) = iter.next
        val docId: Int = next._1
        val info: (String, Double, Double) = next._2
        val name: String = info._1
        val knightRank: Double = info._2
        val Ed: Double = info._3
        writer.write(docId.toString() + " " + knightRank.toString() + " " + Ed.toString() + "\n")
        tWriter.write(docId.toString() + ": " + name + "\n")
      }
      
      tWriter.close()
      writer.close()
    }
   
  
}