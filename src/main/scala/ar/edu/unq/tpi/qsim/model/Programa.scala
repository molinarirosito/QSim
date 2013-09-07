package ar.edu.unq.tpi.qsim.model

import scala.collection.mutable.Buffer
import scala.collection.mutable.ArrayBuffer

case class Programa(var instrucciones:ArrayBuffer[Instruccion]) {
  
var indice = 0

def tamanioDelPrograma() : Int = (instrucciones.map(i => i.tamanioHex).fold(0)(_+_))/4

def siguienteInstruccion():Instruccion = {
  var siguienteInstruccion = instrucciones(indice)
  indice = indice + 1
  siguienteInstruccion
  }

}

object dd extends App{
  var array = ArrayBuffer()
  print("hola".size)
	//	  		   ADD("R0","R3"),
		//  		   MOV("R0","0x0023"),
		  		   //ADD("R0","R3"))
  array.+:(ADD(R1,Inmediato(new W16("0013"))))
//  array.::(MUL("R0","0xF102"))
//  array.::(DIV("R0","0x2453"))
//  array.::(SUB("R0","0x9000"))
//  array.::(MOV("R0","0x1023"))
//  array.::(ADD("R0","0x5000"))
//  array.::(MUL("R0","0x8000"))
  print(array)
  //var p = new Program(array)
  //print(p.instructions)
}
