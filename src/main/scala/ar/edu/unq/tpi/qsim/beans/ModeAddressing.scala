package ar.edu.unq.tpi.qsim.beans

import ar.edu.unq.tpi.qsim.utils.Util

abstract class ModeAddressing {
}

case class Register(var value:String, var number:Int) extends ModeAddressing{
  
  def codeOperation() :String =
  { 
	"100" + Util.toBinary3B(number)
  } 
  
  def getValue() : String = value
  
}

case class Immediate (var value:String) extends ModeAddressing{
  def codeOperation() :String  = "000000" 
  def getValue() : String = value
 
}

object ddd extends App{
  
  var r = Register("23",7)
  print(r.codeOperation())
}