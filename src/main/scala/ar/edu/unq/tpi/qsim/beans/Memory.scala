package ar.edu.unq.tpi.qsim.beans
import scala.collection.mutable.ArrayBuffer
import ar.edu.unq.tpi.qsim.utils._


case class Memory() {
  
  var cells = ArrayBuffer[String]()
  
  def memorySize() : Int = {
    cells.size
  }
  def getValue(pc : String) : String = {
    var current_cel = Util.hexToInteger(pc)
    if(current_cel< this.memorySize())
    {   var value = cells(current_cel)
    	//pc = Util.toHex(current_cel + 1)
    	value    }
    else
    	"This is not a valid memory cell"
  }
  
}

object Testing extends App{
  var memory = Memory()
  memory.cells = ArrayBuffer("1000","1200","1300","1400","1000","1200","1300","1400")
  
  var value = memory.getValue("0001")
  print("el valor es" )
  print("\n")
  print(value)
  value = memory.getValue("0002")
  memory.cells(1)= "ola"
    print("\n")
    print(memory.cells)
}