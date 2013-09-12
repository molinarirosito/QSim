package ar.edu.unq.tpi.qsim.model

import scala.collection.mutable.Buffer
import scala.collection.mutable.ArrayBuffer

case class Programa(var instrucciones:ArrayBuffer[Instruccion_DosOperandos]) {
  
var indice = 0

def tamanioDelPrograma() : Int = (instrucciones.map(i => i.tamanioHex).fold(0)(_+_))/4


def actualizarIndice(){
  indice = indice + 1
}

def obtenerInstruccion():Instruccion_DosOperandos = {
  var siguienteInstruccion = instrucciones(indice)
  actualizarIndice()
  siguienteInstruccion
  }

def finalizo(): Boolean =  { 
   indice == (instrucciones.length )
}

}

object dd extends App{
  var array = ArrayBuffer[Instruccion_DosOperandos]()
  print("hola".size)
  array.append(ADD(R1,Inmediato(new W16("0013"))), ADD(R1,Inmediato(new W16("0013"))))
  print(array)
}
