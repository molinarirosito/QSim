package ar.edu.unq.tpi.qsim.utils

object Util {

  def toHex(number:Int) : String =
{ 
  var new_number = Integer.toHexString(number)
  var new_string = ""
    
    for (x <- new_number.size to 3) {
      new_string = new_string + "0"
    }
  new_string + new_number
 } 
  
  def toInteger(number:String) :Integer =
  {
    Integer.parseInt(number,16)
  }
  
}

object Test extends App{
 
  val t = Util.toHex(2)
  print(t)
}