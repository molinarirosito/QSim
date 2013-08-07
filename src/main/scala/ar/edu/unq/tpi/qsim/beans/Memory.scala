package ar.edu.unq.tpi.qsim.beans
import scala.collection.mutable.ArrayBuffer


class Memory {
  
  var cells = ArrayBuffer[String]()
  var pc = "0"
  
  def getValue() {
    var current_cel = Integer.parseInt(this.pc,16)
    
    
  }
  
}