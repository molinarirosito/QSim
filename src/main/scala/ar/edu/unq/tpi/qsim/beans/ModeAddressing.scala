package ar.edu.unq.tpi.qsim.beans

abstract class ModeAddressing {
}

case class Register(var value:String, var number:Int) extends ModeAddressing{
  
  
def codeOperation() :String =
{ 
  var new_number = number.toBinaryString
  var new_string = "100"
    
    for (x <- new_number.size to 3) {
      new_string = new_string + "0"
    }
   new_string + new_number
 } 
  
   
}

case class Immediate (var value:String) extends ModeAddressing{
  def codeOperation() :String  = "000000"
}