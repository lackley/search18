package search

/**
 * class for ranking pages
 * threshold - value iterations must converge to before 
 *             iterating terminates
 * titleToId - map of titles and their ids
 * pageOutLinks - a list of tuples containing docIds and
 *                the list of titles they link to
 */
class KnightRanker(threshold: Double, titleToId: Map[String, Int], 
                   pageOutLinks: List[(Int, Set[String])]) {
  
  val damp: Double = .15 //dampening factor
  
  /**
     * returns the knight rank of a corpus given a list of docIds and the outgoingLinks
     * to that document
     */
   def getKnightRanks(): Array[Double] = {

    
     //n is size of matrix
     val n: Double = titleToId.size
     
     var matArrA: Array[Array[Double]] = new Array[Array[Double]](n.toInt)
     for(r <- 0 until matArrA.length){
       matArrA(r) = new Array[Double](matArrA.length)
     }
     
     for(outLinks <- pageOutLinks){
       val id: Int = outLinks._1
       val numLinksOut: Double = outLinks._2.size
       var links: Set[String] = outLinks._2
       
       for(outTitle <- links){
         val outId: Option[Int] = titleToId.get(outTitle)
         if(outId != None){
           matArrA(outId.get)(id) = 1.0/numLinksOut
         } else{
           //println("cound not find " + outTitle + " from doc " + id)
         }
       }
     }
     
     
     
     var matArr: Array[Array[Double]] = new Array[Array[Double]](n.toInt)
     for(r <- 0 until n.toInt){
       matArr(r) = new Array[Double](matArr.length)
       for(c <- 0 until n.toInt){
         matArr(r)(c) = (1-damp)* matArrA(r)(c) + damp*(1.0/n)
       }
     }
     
     
     var vecArr: Array[Double] = new Array[Double](matArr.length)
     for(i <- 0 until vecArr.length){
       vecArr(i) = 1/n
     }

     var M: Matrix = new Matrix(matArr)
     
     
    
     
     var R: Vector = new Vector(vecArr)
     var dist: Double = 0
     var curDiff: Double = Double.PositiveInfinity
     //println(M.toString())
     while(curDiff > threshold){
       var lastR = R
       R = M.mult(R)
       R = R.scalarMult(1/R.magnitude())
       curDiff = R.subtract(lastR).magnitude()
     }
     
     var arrR: Array[Double] = R.getArr()
     var sum: Double = 0
     for(i <- 0 until arrR.length){
       sum+= arrR(i)
     }
     
     
     R.scalarMult(1/sum).getArr()
     
   } 

}

object KnightRanker{
  def main(args: Array[String]){
    //tests knight rank on small example
    var map: Map[String, Int] = Map()
    map = map.+(("page1", 0))
    map = map.+(("page2", 1))
    map = map.+(("page3", 2))
    map = map.+(("page4", 3))
    
    var pLinks: List[(Int, Set[String])] = List()
    var s1: Set[String] = Set()
    s1 = s1.+("page2", "page3", "page4")
    var s2: Set[String] = Set()
    s2 = s2.+("page3", "page4")
    var s3: Set[String] = Set()
    s3 = s3.+("page1")
    var s4: Set[String] = Set()
    s4 = s4.+("page1", "page3")
    
    pLinks = (0, s1) :: pLinks
    pLinks = (1, s2) :: pLinks
    pLinks = (2, s3) :: pLinks
    pLinks = (3, s4) :: pLinks
    
    val ranker: KnightRanker = new KnightRanker(.001, map, pLinks) 
    val arr: Array[Double] = ranker.getKnightRanks()
    println(arr.mkString(", "))
    
  }
}