package ar.edu.unq.tpi.qsim.model

class CeldasPuertos {  
  
import scala.collection.mutable.ArrayBuffer
import ar.edu.unq.tpi.qsim.utils.Util
import ar.edu.unq.tpi.qsim.exeptions._

  
  var celdas: ArrayBuffer[W16] = _

/**
 * Devuelve el tamanio de las celdas reservadas a los puertos
 * @return Int 
 */
  def tamanioMemoria(): Int = 239

/**
 * Inicializa cada celda de los puertos con W16 de valor hexadecimal 0000.
 * 
 */
  def initialize() = {
    celdas = new ArrayBuffer[W16]()
    Util.rep(tamanioMemoria()){
      celdas.append(new W16("0000"))
    } 
  }
  
  /**
   * Recibe un entero que debe ser mayor o igual a 65280 ya que a partir de ese numero seria
   * una celda reservada para puertas y retornal el valor de la celda en esa posicion.
   * @param Int
   * @return W16
   */
  def getValor(pc: Int): W16 = {
    if ((pc - 6580) < tamanioMemoria()) {
      var value = celdas(pc)
      value
    } else{
      throw new CeldaFueraDePuertosException("Las celdas reservadas para puertos no llegan a es numero")
      }
  }
  
   /**
   * Recibe un W16 y devuelve el valor de la celda reservada para puertos en esa posicion.
   * @param W16
   * @return W16
   */
  def getValor(pc: W16): W16 = {
    getValor(pc.value)
  }
  
  /**
   * Recibe un String en Hexadecimal y devuelve el valor de la celda reservada para puertos en esa posicion.
   * @param String
   * @return W16
   */
  def getValor(pc: String): W16 = {
    getValor(Util.hexToInteger(pc))
  }
    
  
  /**
   * Pone un valor (W16) en la celda reservada para puertos que se le indica por parametro.
   * @param Int, W16
   */
  def setValorC(celda: Int,dato: W16) = celdas(celda) = dato

   /**
   * Pone un valor (W16) en la celda reservada para puertos que se le indica por parametro en valor hexadecimal.
   * @param String, W16
   */
  def setValor(celda: String, valor: W16) = celdas(Util.hexToInteger(celda)) = valor

  /**
   * Muestra las celdas de puertos imprimiendola a partir de un numero de celda en hexadecimal.
   * @param String
   */
  def show(pc: String): String = {
    var celdas_view = ""
    var pcActual :Int = Util.toInteger(pc)
    
    for (x <- pcActual to tamanioMemoria() - 1) {

      if (x % 7 == 0) {
    	celdas_view = celdas_view + "\n"
      }
      var value = getValor(Util.toHex(pcActual))
      celdas_view = celdas_view + s"[ $value ],"
      pcActual = pcActual + 1
    }
    celdas_view
  }

}