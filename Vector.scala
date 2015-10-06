package search

/**
 * represents a class for a vector of dobules
 */
class Vector(arr: Array[Double]) {

  /**
   * returns the vecotr's array of doubles
   */
  def getArr(): Array[Double] = {
    arr
  }
  
  /**
   * adds two vectors
   * @param v - the vector to add to it
   * @return the result of adding v to this vector
   */
  def add(v: Vector): Vector = {
    var toReturn: Array[Double] = new Array[Double](arr.length)
    for(i <- 0 until arr.length){
      toReturn(i) = arr(i) + v.getArr()(i)
    }
    new Vector(toReturn)
  }
  
  /**
   * subtracts v from this vector
   * @param v - the vector to subtract from
   * @return this vector minus v 
   */
  def subtract(v: Vector): Vector = {
    var toReturn: Array[Double] = new Array[Double](arr.length)
    for(i <- 0 until arr.length){
      toReturn(i) = arr(i) - v.getArr()(i)
    }
    new Vector(toReturn)
  } 
  
  /**
   * multiplies this vector by a scalar
   * @param s - the scalar to multiply by
   * @return this vector times the scalar s
   */
  def scalarMult(s: Double): Vector = {
    var toReturn: Array[Double] = new Array[Double](arr.length)
    for(i <- 0 until arr.length){
      toReturn(i) = arr(i) * s
    }
    new Vector(toReturn)
  }
  
  /**
   * returns the magnitude of this vector
   */
  def magnitude(): Double = {
    var sumSq: Double = 0
    for(i <- 0 until arr.length){
      sumSq += arr(i)*arr(i)
    }
    Math.sqrt(sumSq)
  }
  

    
  override def toString(): String = {
    "[" + arr.mkString(", ") + "]^T"
  }
  
}

object Vector {
  //tests
  def main(args: Array[String]) {
    val a1 = new Vector(Array(1.0,2.0,3.0))
    val a2 = new Vector(Array(7.0,9.0,1.0))
    val a3 = new Vector(Array(2.0,6.0,10.0))
    val a4 = new Vector(Array())
    val a5 = new Vector(Array(1.0))
    val a11 = new Vector(Array(4.0))
    val a6 = new Vector(Array(2.0, 7.0, 4.0, 3.0, 10.0, 11.0, 3.0, 5.0))
    val a7 = new Vector(Array(3.0, 9.0))
    val a8 = new Vector(Array(2.0,10.0,4.0, 5.0, 6.0, 9.0))
    val a9 = new Vector(Array(12.0,3.0,3.0, 0.0, 7.0, 6.0))
    
    println(a1.toString())
    println(a2.toString())
    println(a3.toString())
    println(a4.toString())
    println(a5.toString())
    println(a6.toString())
    println(a7.toString())
    println(a8.toString())
    println(a9.toString())
    
    println(a1.add(a1))
    println(a1.add(a2))
    println(a2.add(a1))
    println(a2.add(a3))
    println(a1.add(a1))
    println(a4.add(a4))
    println(a5.add(a11))
    println(a6.add(a6))
    println(a7.add(a7))
    println(a8.add(a9))
    println(a9.add(a8))
    
    println(a1.subtract(a1))
    println(a1.subtract(a2))
    println(a2.subtract(a1))
    println(a2.subtract(a3))
    println(a1.subtract(a1))
    println(a4.subtract(a4))
    println(a5.subtract(a11))
    println(a6.subtract(a6))
    println(a7.subtract(a7))
    println(a8.subtract(a9))
    println(a9.subtract(a8))
    
    println(a1.scalarMult(4.0))
    println(a1.scalarMult(0.0))
    println(a2.scalarMult(4.0))
    println(a2.scalarMult(0.0))
    println(a3.scalarMult(5.0))
    println(a3.scalarMult(4.0))
    println(a4.scalarMult(10.0))
    println(a5.scalarMult(4.0))
    println(a6.scalarMult(4.0))
    println(a6.scalarMult(0.0))
    println(a7.scalarMult(4.0))
    println(a7.scalarMult(1.0))
    println(a8.scalarMult(2.0))
    println(a9.scalarMult(3.0))
    println(a9.scalarMult(4.0))
    println(a11.scalarMult(4.0))
    
    println(a1.magnitude())
    println(a2.magnitude())
    println(a3.magnitude())
    println(a4.magnitude())
    println(a5.magnitude())
    println(a6.magnitude())
    println(a7.magnitude())
    println(a8.magnitude())
    println(a9.magnitude())
    println(a11.magnitude())
  
  }
}