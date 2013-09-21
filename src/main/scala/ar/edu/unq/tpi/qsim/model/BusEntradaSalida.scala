package ar.edu.unq.tpi.qsim.model
import ar.edu.unq.tpi.qsim.utils.Util

case class BusEntradaSalida {
  
  var memoria: Memoria = _
  var puertos: CeldasPuertos = _
  
  
  def initialize() {
    println("--------INIT------")
    memoria = Memoria(30)
    memoria.initialize
    puertos = CeldasPuertos()
    puertos.initialize
    println("--------TERMINO------")
  }
  
  /**
   * Recibe un entero y devuelve el valor de la celda en memoria o de puertos segun corresponda en esa posicion.
   * @param Int
   * @return W16
   */
  def getValor(pc: Int): W16 = {
    if ((pc >= 65520) && (pc <= 65535)) {
    	this.puertos.getValor(pc)
  }
    else { this.memoria.getValor(pc) }
  }
  
   /**
   * Recibe un W16 y devuelve el valor de la celda en memoria o de puertos segun corresponda en esa posicion.
   * @param W16
   * @return W16
   */
  def getValor(pc: W16): W16 = {
    getValor(pc.value)
  }
  
  /**
   * Recibe un String en Hexadecimal y devuelve el valor de la celda en memoria o de puertos segun corresponda en esa posicion.
   * @param String
   * @return W16
   */
  def getValor(pc: String): W16 = {
    getValor(Util.hexToInteger(pc))
  }
  
  /**
   * Pone un valor (W16) en la celda de memoria o reservada para puertos segun corresponda que se le indica por parametro.
   * @param Int, W16
   */
  def setValorC(celda: Int,dato: W16) = {
    if ((celda >= 65520) && (celda <= 65535)) {
    	this.puertos.setValorC(celda,dato)
  }
    else { this.memoria.setValorC(celda,dato) }
  }

   /**
   * Pone un valor (W16) en la celda de memoria o reservada para puertos segun corresponda que se le indica por parametro en valor hexadecimal.
   * @param String, W16
   */
  def setValor(celda: String, valor: W16) = setValorC(Util.hexToInteger(celda),valor)

}

object purr extends App() {

  var bus = BusEntradaSalida()
  bus.initialize
  println(bus.getValor("FFF0"))
  println(bus.getValor("FFFF"))
  println(bus.getValor("FFF4"))
  println(bus.getValor("000F")) 
  println(bus.getValor("FFEF"))
     
  
}