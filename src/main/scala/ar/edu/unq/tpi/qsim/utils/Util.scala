package ar.edu.unq.tpi.qsim.utils

object Util {

  def toHex(number:Int) : String =
{ 
  var new_number = Integer.toHexString(number)
  var new_string = ""
    
    for (x <- new_number.size to 3) {
      new_string = new_string + "0"
    }
  (new_string + new_number).toUpperCase()
 } 
  
  def toBinary(number:Int) : String =
{ 
  var new_number = Integer.toBinaryString(number)
  var new_string = ""
    
    for (x <- new_number.size to 3) {
      new_string = new_string + "0"
    }
  (new_string + new_number).toUpperCase()
 } 
  
  def hexToInteger(number:String) :Integer =
  {
    Integer.parseInt(number,16)
  }
  
  def binaryToInteger(number:String) :Integer =
  {
    Integer.parseInt(number,2)
  }
  
  
   def binaryToHex(binaryNumber:String) :String =
  {
    
     val value = binaryToInteger(binaryNumber)
     toHex(value)

  }

  
}

object Test extends App{
 
  val t = Util.binaryToHex("111111")

  print(t)
}