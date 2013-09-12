package ar.edu.unq.tpi.qsim.model
import scala.collection.mutable.ArrayBuffer
import ar.edu.unq.tpi.qsim.utils.Util

case class Memoria(var tamanio: Int) {

  var celdas: ArrayBuffer[W16] = _

  def tamanioMemoria(): Int = tamanio

  def initialize() = {
    celdas = new ArrayBuffer[W16]()
    var contador = 0
    do {
      celdas.append(new W16("0000"))
      contador = contador + 1
    } while (contador < tamanioMemoria())
  }
  
  def getValor(pc: String): W16 = {
    var celda_actual = Util.hexToInteger(pc)
    if (celda_actual < Memoria.this.tamanioMemoria()) {
      var value = celdas(celda_actual)
      value
    } else{
      println("Esta no es una celda de memoria valida!!")
      new W16("0000")
      }
  }
    

  
  def cargarPrograma(programa: Programa, inicio: String) {
    var celda_inicio = Util.hexToInteger(inicio)
    val celda_fin = celda_inicio + programa.tamanioDelPrograma
    if (celda_fin < Memoria.this.tamanioMemoria()) {
      for (x <- 0 to programa.instrucciones.size - 1) {
        val instruccion_actual = programa.instrucciones(x)
        insertarInstruccion(celda_inicio, instruccion_actual)
        celda_inicio = celda_inicio + instruccion_actual.cantidadCeldas
      }
    }
  }

  def insertarInstruccion (celda:Int, instruccion: Instruccion)  =
  {
    var string_split = instruccion.representacionHexadecimal().split(" ")
    var celda_actual = celda
    for (x <- 0 to instruccion.cantidadCeldas - 1) {
       setValorC(celda_actual, new W16(string_split(x)))
       celda_actual = celda_actual + 1
      }
  }
  
  def setValorC(celda: Int,dato: W16) = celdas(celda) = dato

  def setValor(celda: String, valor: W16) =
    {
      val numero_celda = Util.hexToInteger(celda)
      celdas(numero_celda) = valor

    }

  def show(pc: String): String = {
    var memoria_view = ""
    var pcActual :Int = Util.toInteger(pc)
    
    for (x <- pcActual to tamanioMemoria() - 1) {

      if (x % 7 == 0) {
    	memoria_view = memoria_view + "\n"
      }
      var value = getValor(Util.toHex(pcActual))
      memoria_view = memoria_view + s"[ $value ],"
      pcActual = pcActual + 1
    }
    memoria_view
  }

}

object Testing extends App {
  var numero = 10 % 10 
  var memory = Memoria(14)
  memory.initialize
  //var string = "2354 4567 0000"
  //var prueba = string.split(" ")
  //println(prueba.toList.toString())
  
  memory.setValorC(0, new W16("3456"))
  memory.setValorC(1, new W16("3456"))
  memory.setValorC(2, new W16("3456"))
  memory.setValorC(3, new W16("3456"))
  println(memory.celdas)
  //println(memory.celdas(0))
  //println(memory.getValor("0000"))
  //memory.setValorC(1, new W16("A222"))
  //memory.setValorC(2, new W16("0004"))
  //memory.setValorC(3, new W16("8996"))
  //memory.setValorC(4, new W16("1234"))
  println(s"" + memory.show("0000"))
  
}
