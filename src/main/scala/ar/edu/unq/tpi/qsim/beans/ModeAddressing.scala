package ar.edu.unq.tpi.qsim.beans

abstract class ModeAddressing (var value_saved:String) {
  
  def codeOperation() : String
  def stringOperation() : String
  override def toString() =  this.stringOperation()
  def toBinaryString() =  this.codeOperation() + "," + value_saved
  def sizeOfCells() : Int = 0
  
}

case class Register(value : String, var number:Int) extends ModeAddressing(value){
  

def stringOperation() :String = "R" + number
def sizeOfCells() : Int = 1

def codeOperation() :String =
{ 
  var new_number = number.toBinaryString
  var new_string = "100"
    
    for (x <- new_number.size to 2) {
      new_string = new_string + "0"
    }
   new_string + new_number
 } 
  
   
}

case class Immediate (value : String) extends ModeAddressing(value){
  def stringOperation() :String = value
  def codeOperation() :String  = "000000"
  def sizeOfCells() : Int = 1
}