package ar.edu.unq.tpi.qsim.beans

import scala.collection.mutable.ArrayBuffer

class Program(var instructions:ArrayBuffer[Instruction]) {
}

object dd extends App{
  var array = new ArrayBuffer[Instruction]()
  array.append(MOV("R0","0x0023"))
  array.append(ADD("R0","R3"))
  array.append(MUL("R0","0xF102"))
  array.append(DIV("R0","0x2453"))
  array.append(SUB("R0","0x9000"))
  array.append(MOV("R0","0x1023"))
  array.append(ADD("R0","0x5000"))
  array.append(MUL("R0","0x8000"))
  
  var p = new Program(array)
  print(p.instructions)
}