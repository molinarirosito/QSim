package ar.edu.unq.tpi.qsim.model
import org.uqbar.commons.utils.Observable


@Observable
case class CeldasPuertos {  
  
  private val NONE = 0
  private val PROGRAM = 1
  private val FECH_DECODE = 2
  private val STORE = 3
  private val EXECUTED = 4
  
import scala.collection.mutable.ArrayBuffer
import ar.edu.unq.tpi.qsim.utils.Util
import ar.edu.unq.tpi.qsim.exeptions._

  
  var celdas: ArrayBuffer[Celda] = _
  
  def celda(pc:Int) = celdas(pc)

/**
 * Devuelve el tamanio de las celdas reservadas a los puertos
 * @return Int 
 */
  def tamanioCeldas(): Int = 16

/**
 * Inicializa cada celda de los puertos con W16 de valor hexadecimal 0000.
 * 
 */
  def initialize() = {
    celdas = new ArrayBuffer[Celda]()
    Util.rep(tamanioCeldas()){
      val w16 = new W16("F000")
      celdas.append(new Celda(w16))
    } 
  }
  
  /**
   * Recibe un entero que debe ser mayor o igual a 65519 ya que a partir de ese numero seria
   * una celda reservada para puertas y retornal el valor de la celda en esa posicion.
   * @param Int
   * @return W16
   */
  def getValor(pc: Int): W16 = {
    val celda_puerto = pc - 65520 
    if (celda_puerto < tamanioCeldas()) {
      var celda = celdas(celda_puerto)
      celda.getW16
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
  def setValorC(celda: Int,dato: W16) = {
      val celda_puerto = celda - 65520 
      if (celda_puerto <  tamanioCeldas()) {
      celdas(celda_puerto).setW16(dato)

    } else{
      throw new CeldaFueraDePuertosException("Las celdas reservadas para puertos no llegan a es numero")
      }
     }
    

   /**
   * Pone un valor (W16) en la celda reservada para puertos que se le indica por parametro en valor hexadecimal.
   * @param String, W16
   */
  def setValor(celda: String, valor: W16) = setValorC(Util.hexToInteger(celda), valor)

  /**
   * Muestra las celdas de puertos imprimiendola a partir de un numero de celda en hexadecimal.
   * @param String
   */
  def show(pc: String): String = {
    var celdas_view = ""
    var pcActual :Int = Util.toInteger(pc)
    
    for (x <- pcActual to tamanioCeldas() - 1) {

      if (x % 7 == 0) {
    	celdas_view = celdas_view + "\n"
      }
      var value = getValor(Util.toHex(pcActual))
      celdas_view = celdas_view + s"[ $value ],"
      pcActual = pcActual + 1
    }
    celdas_view
  }
  
   /**
   * Cambia el estado de una celda reservada para puertos por el pasado por parametro.
   * @param Int, Int
   */
  def setStateCelda(num_celda: Int,state: CeldaState.Type) = celdas(num_celda).state = state

}