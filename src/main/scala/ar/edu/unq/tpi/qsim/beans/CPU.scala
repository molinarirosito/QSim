package ar.edu.unq.tpi.qsim.beans

import scala.collection.mutable.ArrayBuffer
import ar.edu.unq.tpi.qsim.utils.Util

case class CPU() {

  var registers = ArrayBuffer[Register]()+=(R0,R1,R2,R3,R4,R5,R6,R7)
  var pc = Util.toHex(0)
  var ir = ""  
  var alu = new ALU() 
  
  def incrementPc(){
   // pc = Util.
  }

}

object pur extends App(){
  
  var cpu = CPU()
  print(cpu.registers +"\n")
  print(cpu.pc)
}