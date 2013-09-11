package ar.edu.unq.tpi.qsim.model
import scala.collection.mutable.ArrayBuffer
import ar.edu.unq.tpi.qsim.utils._

case class Memoria(var tamanio: Int) {

  var celdas: ArrayBuffer[W16] = _

  def tamanioMemoria(): Int = tamanio

  def initialize() = {
    celdas = new ArrayBuffer[W16](tamanio)
    var contador = 0
    do {
      celdas += (new W16("0000"))
      contador = contador + 1
    } while (contador < tamanioMemoria())
  }
  
  def getValor(pc: String): W16 = {
    var celda_actual = Util.hexToInteger(pc)
    if (celda_actual < Memoria.this.tamanioMemoria()) {
      var value = celdas(celda_actual)
      //pc = Util.toHex(current_cel + 1)
      value
    } else
      "Esta no es una celda de memoria valida!!"
      new W16("0000")
  }


  def cargarPrograma(programa: Programa, inicio: String) {
    //Quiero hacerlo de esta forma pero tengo que pensar bien las cosas
    //programa.instrucciones.foreach(instruccion => setValor(pc, instruccion.representacionHexadecimal()))

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
    val string_split = instruccion.representacionHexadecimal().split(" ")
    var celda_actual = celda
    for (x <- 0 to instruccion.cantidadCeldas - 1) {
       setValorC(celda_actual, new W16(string_split(x)))
       celda_actual = celda_actual + 1
      }
    
  }
  def setValorC(celda: Int, valor: W16) =
    { celdas(celda) = valor }

  def setValor(celda: String, valor: W16) =
    {
      val numero_celda = Util.hexToInteger(celda)
      celdas(numero_celda) = valor

    }

  def show(pc: W16): String = {
    var memoria_view = ""
    var pcActual: W16 = pc
    
    for (x <- pcActual.value to tamanioMemoria() - 1) {
       // aca indico el limite de celdas por linea en el print
       //Pasado este limite escribe en la liniea siguiente.
      if (x % 7 == 0) {
    	memoria_view = memoria_view + "\n"
      }
      // busco el valor de la celda
      var value = getValor(pcActual.toString)
      // Agrego la celda junto con su valor
      memoria_view = memoria_view + s"[ $value ],"
      // paso a la celda siguiente
      pcActual = pcActual.+(new W16("0001"))
    }
    // Devuelvo la memoria armada.
    memoria_view
  }

}

object Testing extends App {
  var numero = 10 % 10 
  var memory = Memoria(14)
  memory.initialize
  memory.setValor("0000", new W16("3456"))
  memory.setValor("0001", new W16("A222"))
  memory.setValor("0002", new W16("0004"))
  memory.setValor("0003", new W16("8996"))
  memory.setValor("0004", new W16("1234"))
  println(s"" + memory.show(new W16("0000")))
  
  //  var a = 0
  //   memory.initialize
  //  println(memory.celdas)
  //   println(memory.celdas.size)
  //  
  /*memory.cells = ArrayBuffer("1000","1200","1300","1400","1000","1200","1300","1400")
  
  var value = memory.getValue("0001")
  print("el valor es" )
  print("\n")
  print(value)
  value = memory.getValue("0002")
  memory.cells(1)= "ola"
    print("\n")
    print(memory.cells)*/

}
