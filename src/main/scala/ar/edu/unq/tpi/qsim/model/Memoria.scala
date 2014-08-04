package ar.edu.unq.tpi.qsim.model

import scala.collection.mutable.ArrayBuffer
import ar.edu.unq.tpi.qsim.utils.Util
import ar.edu.unq.tpi.qsim.exeptions._
import org.uqbar.commons.utils.Observable

@Observable
case class Memoria(var tamanio: Int) {

  var celdas: ArrayBuffer[Celda] = _

  def celda(pc: Int) = celdas(pc)

  def celda(pc: Int, w16: W16): Unit = celda(pc).value = w16

  /**
   * Devuelve el tamanio de la memoria
   *
   */
  def tamanioMemoria(): Int = tamanio

  /**
   * Inicializa cada celda de la memoria con W16 de valor hexadecimal 0000.
   *
   */
  def initialize() = {
    celdas = new ArrayBuffer[Celda]()
    var contador = 0
    do {
      val w16 = new W16("0000")
      celdas.append(new Celda(w16))
      contador = contador + 1
    } while (contador < tamanioMemoria())
  }

  /**
   * Recibe un entero y devuelve el valor de la celda en memoria en esa posicion.
   * @param Int
   * @return W16
   */
  def getValor(pc: Int): W16 = {
    if (pc < Memoria.this.tamanioMemoria()) {
      var celda = celdas(pc)
      celda.getW16
    } else {
      throw new CeldaFueraDeMemoriaException("La memoria no tiene ese numero de celda")
    }
  }

  /**
   * Recibe un entero y devuelve el valor de la celda en memoria en esa posicion.
   * @param Int
   * @return W16
   */
  def getCeldas(pc: Int, cant_celdas: Int): ArrayBuffer[Celda] = {
    var las_celdas = ArrayBuffer[Celda]()
    for (i ← 0 to cant_celdas - 1) {
      las_celdas += getCelda(pc + i)
    }
    las_celdas
  }

  /**
   *
   */
  def getCelda(pc: Int): Celda = {
    if (pc < Memoria.this.tamanioMemoria()) {
      celdas(pc)
    } else {
      throw new CeldaFueraDeMemoriaException("La memoria no tiene ese numero de celda")
    }
  }

  /**
   * Recibe un W16 y devuelve el valor de la celda en memoria en esa posicion.
   * @param W16
   * @return W16
   */
  def getValor(pc: W16): W16 = {
    getValor(pc.value)
  }

  /**
   * Recibe un String en Hexadecimal y devuelve el valor de la celda en memoria en esa posicion.
   * @param String
   * @return W16
   */
  def getValor(pc: String): W16 = {
    getValor(Util.hexToInteger(pc))
  }

  /**
   * Carga un programa en memoria desde el numero de celda en Hexadecimal que se le envie como inicio.
   * @param String, Programa
   */
  def cargarPrograma(programa: Programa, inicio: String) {
    var celda_inicio = Util.hexToInteger(inicio)
    val celda_fin = celda_inicio + programa.tamanioDelPrograma
    if (celda_fin < Memoria.this.tamanioMemoria()) {
      for (x ← 0 to programa.instrucciones.size - 1) {
        val instruccion_actual = programa.instrucciones(x)
        insertarInstruccion(celda_inicio, instruccion_actual)
        celda_inicio = celda_inicio + instruccion_actual.cantidadCeldas
      }
    }
  }

  /**
   * Inserta una instruccion en memoria a partir de la celda que se le indica.
   * @param Int, Instruccion
   */
  def insertarInstruccion(celda: Int, instruccion: Instruccion) =
    {
      var string_split = instruccion.representacionHexadecimal().split(" ")
      var celda_actual = celda
      for (x ← 0 to instruccion.cantidadCeldas - 1) {
        setValorC(celda_actual, new W16(string_split(x)))
        setStateCelda(celda_actual, State.PROGRAM)

        celda_actual = celda_actual + 1
      }
    }

  /**
   * Cambia el estado de una celda por el pasado por parametro.
   * @param Int, Int
   */
  def setStateCelda(num_celda: Int, state: State.Type) = celdas(num_celda).state = state

  /**
   * Pone un valor (W16) en la celda que se le indica por parametro.
   * @param Int, W16
   */
  def setValorC(num_celda: Int, dato: W16) = {
    if (num_celda < Memoria.this.tamanioMemoria()) {
      celdas(num_celda).value = dato
    } else {
      throw new CeldaFueraDeMemoriaException("La memoria no tiene ese numero de celda")
    }
    }
    

  /**
   * Pone un valor (W16) en la celda que se le indica por parametro en valor hexadecimal.
   * @param String, W16
   */
  def setValor(celda: String, valor: W16) = setValorC(Util.hexToInteger(celda), valor)

  /**
   * Muestra la memoria imprimiendola a partir de un numero de celda en hexadecimal.
   * @param String
   */
  def show(pc: String): String = {
    var memoria_view = ""
    var pcActual: Int = Util.toInteger(pc)

    for (x ← pcActual to tamanioMemoria() - 1) {

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
  //println(s"" + memory.show("0000"))

}
