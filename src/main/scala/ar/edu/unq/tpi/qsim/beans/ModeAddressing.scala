package ar.edu.unq.tpi.qsim.beans

import ar.edu.unq.tpi.qsim.utils.Util

trait ModeAddressing {
  //DELEGAR CAMBIO A EXADECIMAL A LA INSTRUCCION, SE SOLAPAN LOS MODOS DE DIRECCIONAMIENTO!!!

  def code() : String
  def stringOperation() : String
  override def toString() =  this.stringOperation()
  def getStringValue() : String
  def bits() : Int = 0
}

abstract class Register() extends ModeAddressing{

var value : String = "0000"

def number(): Int
def stringOperation() :String = "R" + number
override def bits() : Int = 6
override def getStringValue() : String = ""

def code() :String =
{ 
  "100" + Util.toBinary3B(number)
}
}

object R0 extends Register()
{ 
  override def number : Int = 0
}
object R1 extends Register()
{ 
  override def number : Int = 1
}
object R2 extends Register()
{ 
  override def number : Int = 2
}
object R3 extends Register()
{ 
  override def number : Int = 3
}
object R4 extends Register()
{ 
  override def number : Int = 4
}
object R5 extends Register()
{ 
  override def number : Int = 5
}
object R6 extends Register()
{ 
  override def number : Int = 6
}
object R7 extends Register()
{ 
  override def number : Int = 7
}


case class Immediate (value : String) extends ModeAddressing{
  override def getStringValue() : String = value
  def stringOperation() :String = value
  def code() :String  = "000000"
 // override def bits() : Int = this.toBinaryString.size - 1
}
