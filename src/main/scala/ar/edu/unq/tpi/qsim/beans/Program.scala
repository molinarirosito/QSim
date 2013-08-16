package ar.edu.unq.tpi.qsim.beans

import scala.collection.mutable.Buffer
import scala.collection.mutable.ArrayBuffer

case class Program(var instructions:ArrayBuffer[Instruction]) {
  
	
def sizeOfProgram() : Int = (instructions.map(i => i.sizeOfbits).fold(0)(_+_))/4
 
}

object dd extends App{
  var array = ArrayBuffer()
  print("hola".size)
	//	  		   ADD("R0","R3"),
		//  		   MOV("R0","0x0023"),
		  		   //ADD("R0","R3"))
  array.+:(ADD(R1,Immediate("0013")))
//  array.::(ADD("R0","R3"))
//  array.::(MUL("R0","0xF102"))
//  array.::(DIV("R0","0x2453"))
//  array.::(SUB("R0","0x9000"))
//  array.::(MOV("R0","0x1023"))
//  array.::(ADD("R0","0x5000"))
//  array.::(MUL("R0","0x8000"))
  
  //var p = new Program(array)
  //print(p.instructions)
}
