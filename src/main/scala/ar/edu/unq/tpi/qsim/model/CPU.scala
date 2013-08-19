package ar.edu.unq.tpi.qsim.model

import scala.collection.mutable.ArrayBuffer
import ar.edu.unq.tpi.qsim.utils.Util

case class CPU() {

  var registros = ArrayBuffer[Registro](R0,R1,R2,R3,R4,R5,R6,R7)
  var flags = ArrayBuffer[Int](0,0,0,0)
  var pc = Util.toHex(0)
  var ir = ""  
  var alu = new ALU() 
  
  def incrementarPc(){
   // pc = Util.
  }

}

object pur extends App(){
  
  var cpu = CPU()
  print(cpu.registros +"\n")
  print(cpu.pc)
}