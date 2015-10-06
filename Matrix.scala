package search

/**
 * represents matrix of doubles
 */
class Matrix(entries: Array[Array[Double]]) {

  /**
   * multiplies a matrix with a vector, returns Ax
   * @param v the vector to multiply A by
   * @return the vector result of Av
   */
  def mult(v: Vector): Vector = {
    var toReturn: Array[Double] = new Array[Double](v.getArr.length)
     for(r <- 0 until entries.length){
       var row: Array[Double] = entries(r)
       var dotProd: Double = 0
       for(c <- 0 until row.length){
         dotProd += row(c)*v.getArr()(c)
       }
       toReturn(r) = dotProd
     }
     new Vector(toReturn)
  }
  
  override def toString(): String = {
    val builder: StringBuilder = new StringBuilder()
    for(r <- 0 until entries.length){
      for(c <- 0 until entries(r).length){
        builder.append(entries(r)(c) + " | ")
      }
      builder.append("\n")
    }
    builder.toString()
  } 
}

object Matrix {
  def main(args: Array[String]){
    val entries1 = new Matrix(Array(Array(1.0, 2.0, 3.0), Array(2.0, 1.0, 2.0), Array(3.0,3.0,3.0)))
    val entries2 = new Matrix (Array(Array()))
    val entries3 = new Matrix(Array(Array(2.0, 2.0, 1.0), Array(7.0, 1.0, 10.0), Array(1.0,1.0,1.0)))
    val entries4 = new Matrix (Array(Array(1.0, 1.0), Array(2.0, 2.0)))
    val entries5 = new Matrix (Array(Array(1.0, 2.0, 3.0,10.0), Array(2.0, 7.0, 1.0, 2.0), 
        Array(9.0, 3.0,3.0,3.0), Array(2.0, 5.0, 8.0,3.0)))
    val v1 = new Vector (Array(1.0, 1.0, 1.0))
    val v2 = new Vector(Array(2.0, 3.0, 5.0))
    val v3 = new Vector(Array(7.0,2.0,1.0))
    val v4 = new Vector(Array(9.0, 1.0, 10.0, 11.0))
    val v5 = new Vector(Array(10.0, 11.0, 1.0, 0.0))
    val v6 = new Vector(Array(15.0, 16.0, 2.0, 9.0))
    val v7 = new Vector(Array(1.0, 1.0))
    val v8 = new Vector(Array(4.0, 3.0))
    println(entries1.mult(v1).toString())
    println(entries1.mult(v2).toString())
    println(entries1.mult(v3).toString())
    println(entries2.mult(v1).toString())
    println(entries2.mult(v2).toString())
    println(entries3.mult(v1).toString())
    println(entries3.mult(v2).toString())
    println(entries3.mult(v3).toString())
    println(entries4.mult(v7).toString())
    println(entries4.mult(v8).toString())
    println(entries5.mult(v4).toString())
    println(entries5.mult(v5).toString())
    println(entries5.mult(v6).toString())
    println(entries4.mult(v6).toString())
    
    
}
  }
