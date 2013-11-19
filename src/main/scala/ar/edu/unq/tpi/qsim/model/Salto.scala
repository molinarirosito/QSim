package ar.edu.unq.tpi.qsim.model
import ar.edu.unq.tpi.qsim.utils._

class Salto(var salto: Int) {

  var isNegative = false
  /**
   * Devuelve el numero que tiene el salto en String
   * @return String
   */
  override def toString() = value.toString

  /**
   * Devuelve la representacion binaria del numero que tiene el salto
   * @return String
   */
  def toBinary: String = Util.toBinary8B(salto)

  /**
   * Devuelve si es incrementar o decrementar el desplazamiento
   * @return Int
   */
  def signo: Boolean = isNegative

  /**
   * Devuelve el numero que tiene el salto
   * @return Int
   */
  def value: Int = salto

  def this(binary_chain: String) {
    this(Util.binaryToInteger(binary_chain));

  }
}

class SaltoEtiqueta(var etiqueta: ModoDireccionamiento) extends Salto(0) {
}