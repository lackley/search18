package search
import scala.util.matching.Regex
import scala.io.Source._ 
import scala.collection.mutable.PriorityQueue
import java.util.Scanner
import java.io.FileInputStream

//Query object runs query program through terminal
object Query {
  
  //runs the query program through the terminal
  def main(args: Array[String]) {
    //testMed()
    var smart: Boolean = false
    var knightRank: Boolean = false
    if(args.length < 3){
      println("Must give query at least 3 file names.")
    } else if(args(0) == "--smart"){
      smart = true
    } else if(args(1) == "--knight-rank"){
      knightRank = true
    }
    var title: String = ""
    var doc: String = ""
    var word: String = ""
    var invalid: Boolean = false
    if(args.length == 3) {
      title = args(0)
      doc = args(1)
      word = args(2)
    } else if(args.length == 4){
      if(args(0) == "--knight-Rank"){
        knightRank = true
      }
      title = args(1)
      doc = args(2)
      word = args(3)
    } else if(args.length == 5){
      title = args(2)
      doc = args(3)
      word = args(4)
    } else {
      println("Invalid arguments given to querier.")
      invalid = true
    }
    
    if(!invalid){
      val query: Query = new Query(smart, knightRank, title, doc, word)
      val validDoc = query.buildDocTable()
      val validWord = query.buildWordTable()
      if(validDoc && validWord){
        println("enter search phrase: ")
        var line: String = readLine()
        var quitBool: Boolean = false
        while(!quitBool && line != null){
          if(line == ":quit"){
          quitBool = true
          } else {
            val answer: String = query.answerQuery(line)
            println(answer)
            println("enter search phrase: ")
            line = readLine()
          }
        }
      }
    }

    
    
  }
  
  //test on the most basic test.xml
  def test() = {
    val doc: String = "/gpfs/main/home/lackley/course/cs018/" + 
                      "workspace/scalaproject/src/search/docFile4.txt"
    val word: String = "/gpfs/main/home/lackley/course/cs018/" + 
                       "workspace/scalaproject/src/search/wordFile4.txt"
    val title: String = "/gpfs/main/home/lackley/course/cs018/" + 
                        "workspace/scalaproject/src/search/titleFile4.txt"
    val q: Query = new Query(false, false, title, doc, word)
    q.buildDocTable()
    q.buildWordTable()
    println(q.wordTable)
    println(q.answerQuery("good page"))
  }
  
  //test on small wiki
  def testSmall() = {
    
    val doc: String = "/gpfs/main/home/lackley/course/cs018/" + 
                      "workspace/scalaproject/src/search/docFileSmall.txt"
    val word: String = "/gpfs/main/home/lackley/course/cs018/" + 
                       "workspace/scalaproject/src/search/wordFileSmall2.txt"
    val title: String = "/gpfs/main/home/lackley/course/cs018/" + 
                        "workspace/scalaproject/src/search/titleFileSmall.txt"
    val q: Query = new Query(false, true, title, doc, word)
    q.buildDocTable()
    q.buildWordTable()
    println(q.answerQuery("philosophy"))
    
  }
  
    //test on small wiki
  def testMed() = {
    
    val doc: String = "/gpfs/main/home/lackley/course/cs018/" + 
                      "workspace/scalaproject/src/search/docFileMed.txt"
    val word: String = "/gpfs/main/home/lackley/course/cs018/" + 
                       "workspace/scalaproject/src/search/wordFileMed.txt"
    val title: String = "/gpfs/main/home/lackley/course/cs018/" + 
                        "workspace/scalaproject/src/search/titleFileMed.txt"
    val q: Query = new Query(false, true, title, doc, word)
    q.buildDocTable()
    q.buildWordTable()
    println(q.answerQuery("John Lennon"))
    
  }
  
}
/**
 * represents a class for querying a corpus of wiki pages
 * @param smartBool - true for implementing "smart" 
 * @param knightRankBool - true if you want to use knightRank
 * @param titleFile - name of file containing titles and ids
 * @param docFile - name of file containg docids, knight ranks, 
 *                   and euclidian distances
 * @param wordFile - name of file containing every word in
 *                   the corpus followed by the id of doc
 *                   and frequency of word in doc for all
 *                   docs that contain the word
 */
class Query(smartBool: Boolean, knightRankBool: Boolean, 
            titleFile: String, docFile: String, wordFile: String) {
  
    //the number of results to display to a user
    val NumTopDocs = 10

    //word table is a hashtable, the keys are every word in corpus
    //the value is a tuple containg the inverse word frequency in entire corpus
    //                                  -this is different from index table 
    //and a list that contains the frequency 
    // of the word in all documents it appears
    //calling map.get(docId) should give the number of 
    // times the word appears in that doc
    //                 word    idfw        docId fdw
    var wordTable: Map[String, (Double, Map[Int, Int])] = Map()
    
    //doc info is a map that for each document contains the name,
    //knightRank, euclidian norm
    var docInfo: Map[Int, (String, Double, Double)] = Map()
    
    /**
     * builds the docInfo hash table
     * @return true if the table was built sucessfully, 
     *         false if the file could not be found
     */
    def buildDocTable(): Boolean = {
      
      var isValid: Boolean = true
      var kRankList: List[Double] = List()
      
      var docIter: Iterator[String] = null
      var titleIter: Iterator[String] = null
      
      try{
        docIter = fromFile(docFile).getLines()
      }catch{
        case e: Exception => { println("Cound not find " + docFile)
                               isValid = false }
        
      }
      try{
        titleIter = fromFile(titleFile).getLines()
      }catch{
        case e: Exception =>  { println("Cound not find " + titleFile)
                                isValid = false } 
      }
                         

      if(isValid){
        
        while(docIter.hasNext){
          val next: String = docIter.next
          val titleNext: String = titleIter.next
        
          val splitIndex: Int = titleNext.indexOf(":")
          val title: String = titleNext.substring(splitIndex+2)
        
          val strArr: Array[String] = next.split(" ")
          if(strArr.length != 3){
            println("the files were not properly formatted")
          } else {
            val docIDStr: String = strArr(0)
            //TODO: deal with illegal number formatting
            val docID: Int = docIDStr.trim().toInt
            val kRank: Double = strArr(1).trim().toDouble
            val Ed: Double = strArr(2).trim().toDouble
            docInfo = docInfo.+((docID, (title, kRank, Ed)))
            kRankList = kRank :: kRankList
          }
        }
      }
      isValid
    }
    
        /**
     * builds the wordTable
     * @return true if the hash table       
     *         false if the file could not be found
     */
    def buildWordTable(): Boolean = {

      var isValid: Boolean = true
      var iter: Iterator[String] = null
      try{

        iter = fromFile(wordFile).getLines()
      }catch{
        case e: Exception =>  { println("Cound not find " + wordFile)
                                isValid = false } 
      }  
      
      if(isValid){
        while(iter.hasNext){
          val next: String = iter.next
          wordTable = wordTable.+(parseWordLine(next))
        }
      }
      isValid
    }
   
    def parseWordLine(next: String): (String, (Double, Map[Int, Int])) = {
      val lastQuote: Int = next.indexOf("\"",1)
      val word: String = next.substring(1, lastQuote)
      val colon: Int = next.indexOf(":")
      val idfwStr: String = next.substring(lastQuote+2, colon-1)
      val idfw: Double = idfwStr.toDouble
      val rest: String = next.substring(colon+2)
      var docMap: Map[Int, Int] =Map()
      val restSplit: Array[String] = rest.split(" ")
      var counter: Int = 0
      while(counter < restSplit.length){
         val id: Int = restSplit(counter).toInt
         val freq: Int = restSplit(counter + 1).toInt
         docMap = docMap.+((id, freq))
         counter += 2
      }
      (word, (idfw, docMap))
    }
   
    
    /**
     * answers a user's query
     * @param input - the user's input
     * @return a string that is the top documents
     *         for the user's query
     */
    def answerQuery(input: String): String = {
      
      val builder: StringBuilder = new StringBuilder()
      val stems: Array[String] = stemQuery(input)
      
      val docScores: PriorityQueue[(Double, Document)] = getDocScores(stems)
     
      
      var numDocs: Int = NumTopDocs
      if(docScores.length < NumTopDocs){
        numDocs = docScores.length
      }
      if(numDocs == 0){
        builder.append("No documents match your query. Try checking spelling.")
      }
      for(i <- 1 to numDocs){
        val docAndScore: (Double, Document) = docScores.dequeue()
        val doc = docAndScore._2
        val score = docAndScore._1
        
        builder.append(i)
        builder.append(") ")
        builder.append(doc.getTitle())
        builder.append("\n")
      }
      builder.toString
    }
    
    /**
     * private class representing a word in a document
     * @param word - the word itself
     * @param idfw - the inverse document frequency of the word
     * @param fdw - the frequency of the word in the document
     */
    private class WordInDoc(word: String, idfw: Double, fdw: Double){
      
      //returns the inverse document frequency of this word
      def getIdfw(): Double = {idfw}
      
      //returns the frequency of the word in respective document
      def getFdw(): Double = {fdw}
      
      override def toString(): String = {
        word
      }
    }
    
    /**
     * private class representing a document 
     * @param id - the id of the doc
     * @param title - the title of the doc
     * @param myWords - a list of words in the doc that are in query
     * @param kRank - the knightRank of a document
     * @param Ed- the euclidian distance of the document
     */
    private class Document(id: Int, title: String, myWords: List[WordInDoc], 
                           kRank: Double, Ed: Double){
      
      //list of words that are both in document and query
      private var words: List[WordInDoc] = myWords
      
      //gets the doc's id
      def getId(): Int = {id}
      
      //gets the doc's title
      def getTitle(): String = {title}
      
      //gets the words in the document that re in user's query
      def getWords(): List[WordInDoc] = {myWords}
      
      def getKnightRank(): Double = {kRank}
      
      def getEd(): Double = {Ed}
      
      /**
       * scores a document 
       * @param withKRank - whether or not to use knightRank
       * @return the document's score
       */
      def scoreDoc(withKRank: Boolean): Double = {
        var score: Double = 0
        
        for(word <- myWords){
          score += word.getIdfw() * (word.getFdw() / Ed)
        }
        if(withKRank){ 
          score * kRank
        } else {
          score
        }
      }
      
      //adds a word to words in document and query
      def addWord(word: WordInDoc) {
        //println(word +  " was added")
       // println("document " + id + " has " + words.length + " words")
        words = word :: words
      }
      
      
      override def equals(o: Any): Boolean = {
        if(!o.isInstanceOf[Document]){
          false
        } else {
          val otherDoc: Document = o.asInstanceOf[Document]
          (this.id == otherDoc.getId())
        }
      }
    }
    
    /**
     * gets rid of punctuation, capitalization, and stems a query
     * @param input - the user's input to be stemmed
     * @return an array of all words in input stemmed
     */
    def stemQuery(input: String): Array[String] = {
      val inputLC = input.toLowerCase()
      val regex = new Regex("""\[\[[^\[]+?\]\]|[^\W\_]+'[^\W\_]+|[^\W\_]+""")
      val matchIterator = regex.findAllMatchIn(inputLC)
      val strList = matchIterator.toList.map { aMatch => aMatch.matched }
      val strArr: Array[String] = PorterStemmer.stemArray(strList.toArray)
      strArr
    }
    
    
    /**
     * returns a hash map of all documents that contain any words in query
     * @param words - the stemmed words of a user's query
     */
    private def getValidDocs(words: Array[String]): Map[Int, Document] = {
      
      var validDocs: Map[Int, Document] = Map()
      
      for(word <- words){
        val wordTableResult: Option[(Double, Map[Int, Int])] = 
                                              wordTable.get(word)
        if(wordTableResult != None){
          val idfw: Double = wordTableResult.get._1
          val iter = wordTableResult.get._2.iterator
          while(iter.hasNext) {
            val next: (Int, Int) = iter.next
            val docId: Int = next._1
            val fdw: Double = next._2.toDouble
            val wordInDoc: WordInDoc = new WordInDoc(word, idfw, fdw)
            val existingDoc: Option[Document] = validDocs.get(docId)
            if(existingDoc != None){
              //if you are already considering this doc for query, 
              //just add the word
              //and put it back in validDocs
              val doc: Document = existingDoc.get
              //println("adding " + wordInDoc.toString() + " to doc " + docId)
              
              var words: List[WordInDoc] = doc.getWords
              
              words = wordInDoc :: words
              
              val newDoc: Document = new Document(doc.getId(), doc.getTitle(), words, doc.getKnightRank(), doc.getEd())
              validDocs = validDocs.+((docId, newDoc))
              
            } else {
              //if this word is the first time this document appears, look up 
              //its information in docInfo, create the doc, add it to validDocs
              //with wordInDoc as its only word in myWords
              val dInfo: Option[(String, Double, Double)] = docInfo.get(docId)
              if(dInfo != None){
                val info: (String, Double, Double) = dInfo.get
                val docName: String = info._1
                val kRank: Double = info._2
                val Ed: Double = info._3
                val myWords: List[WordInDoc] = List(wordInDoc)
                val doc: Document = new Document(docId, docName, 
                                                 myWords, kRank, Ed)
                validDocs = validDocs.+((docId, doc))
              }
            }
            
            
          }
          
        }
        
      }
      
      validDocs
    }
    
    
    /**
     * scores the documents and puts them in a priority queue based
     * on documents with top scores
     * @param words- the stemmed words the user inputs
     * @return a priority queue of doc scores and documents
     */
    private def getDocScores(words: Array[String]): 
                  PriorityQueue[(Double, Document)] = {
      
      def score(tup: (Double, Document)) = tup._1
      
      val pQueue = new PriorityQueue[(Double, Document)]()(Ordering.by(score))
      
     
      val validDocs: Map[Int, Document] = getValidDocs(words)
      
      val iter = validDocs.iterator
      var docsAndScores: List[(Double, Document)] = List()
      while(iter.hasNext){
        val next: (Int, Document) = iter.next
        val doc: Document = next._2
        val docScore: (Double, Document) = (doc.scoreDoc(knightRankBool), doc)
        pQueue.+=(docScore)
      }
      pQueue
      
    }
    
    
  
}