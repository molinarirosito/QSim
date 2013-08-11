package ar.edu.unq.tpi.qsim.beans
import scala.collection.mutable.ArrayBuffer
import ar.edu.unq.tpi.qsim.utils.Util


case class Memory() {
  
  var cells = ArrayBuffer[String]()
  var pc = "0000"
  
  def memorySize() : Int = {
    cells.size
  }
  def getValue() : String = {
    var current_cel = Util.toInteger(this.pc)
    if(current_cel< this.memorySize())
    {   var value = cells(current_cel)
    	pc = Util.toHex(current_cel + 1)
    	value    }
    else
    	"This is not a valid memory cell"
  }
  
}

object Testing extends App{
  var memory = Memory()
  memory.cells = ArrayBuffer("1000","1200","1300","1400","1000","1200","1300","1400")
  memory.pc = "0001"
  var value = memory.getValue()
  print("el valor es" )
  print("\n")
  print(value)
  value = memory.getValue()
  memory.cells(1)= "ola"
    print("\n")
    print(memory.cells)
}