package ar.edu.unq.tpi.qsim.model

import ar.edu.unq.tpi.qsim.utils.Util

trait ModoDireccionamiento {

  def getValor() : W16
  def codigo() : String
  def representacionString() : String
  override def toString() =  this.representacionString()
  def getValorString() : String
  def bits() : Int = 0
}

/** REGISTROS **/
abstract class Registro() extends ModoDireccionamiento{

var valor : W16 = new W16("0000")

def numero(): Int

/**
 * Este mensaje devuelve el nombre del registro con su respectivo numero: Rx 
 * con x de [0..7]
 * @return String
 */
def representacionString() :String = "R" + numero

/**
 * Devuelve la cantidad de bits que ocupa este modo de direccionamiento
 * @return Int
 */
override def bits() : Int = 6
override def getValorString() : String = ""

/**
 * Devuelve el valor que tiene el Registro
 * @return W16
 */  
def getValor() : W16 = valor

/**
 * Cambia el valor que tiene el Registro por el pasado por parametro
 * @parameters un_valor : W16
 * @return W16
 */ 
def setValor(un_valor : W16){
  valor = un_valor
}

/**
 * Devuelve el codigo de modo de direccionamiento del registro: 
 * 100xxx siendo xxx un numero en binario del 0 al 7 que concuerda con el
 * numero de registro
 * @return String
 */ 
def codigo() :String =
{ 
  "100" + Util.toBinary3B(numero)
}
}

object R0 extends Registro() { override def numero : Int = 0 }
object R1 extends Registro() { override def numero : Int = 1 }
object R2 extends Registro() { override def numero : Int = 2 }
object R3 extends Registro() { override def numero : Int = 3 }
object R4 extends Registro() { override def numero : Int = 4 }
object R5 extends Registro() { override def numero : Int = 5 }
object R6 extends Registro() { override def numero : Int = 6 }
object R7 extends Registro() { override def numero : Int = 7 }

case class RegistroIndirecto (registro : Registro) extends ModoDireccionamiento{
  
  /**
   * Devuelve el valor que tiene el registro indirecot
   * @return W16
   */
  def getValor() : W16 = registro.getValor
 
  /**
   * Devuelve el String del valor que tiene el registro indirecot
   * @return String
   */
  override def getValorString() : String = registro.getValorString
  
  /**
   * Este mensaje devuelve el nombre del registro indirecto con su respectivo numero: 
   * [Rx] con x de [0..7]
   * @return String
   */
  def representacionString() :String = "[" + registro.representacionString + "]"
  
 /**
   * Devuelve el codigo de modo de direccionamiento del registro: 
   * 110xxx siendo xxx un numero en binario del 0 al 7 que concuerda con el
   * numero de registro
   * @return String
   */ 
  def codigo() :String  = "110" +  Util.toBinary3B(registro.numero)
 // override def bits() : Int = this.toBinaryString.size - 1
}

case class Inmediato (valor : W16) extends ModoDireccionamiento{
  
  /**
   * Retorna el valor del inmediato
   * @return W16
   */
  def getValor() : W16 = valor
  
  /**
   * Retorna el valor del inmediato en String
   * @return String
   */
  override def getValorString() : String = valor.toString
  
  /**
   * Retorna la representacion del modo de direccionamiento
   * inmediato en String XXXX, un valor hexadecimal.
   * @return W16
   */
  def representacionString() :String = valor.toString
  
 /**
   * Devuelve el codigo de modo de direccionamiento del modo de direccionamiento: 
   * @return String
   */ 
  def codigo() :String  = "000000"
 // override def bits() : Int = this.toBinaryString.size - 1
}

case class Directo (inmediato : Inmediato) extends ModoDireccionamiento{
  
  /**
   * Devuelve el valor que tiene el modo de direccionamiento Directo
   * @return W16
   */
  def getValor() : W16 = inmediato.getValor
  
  /**
   * Devuelve el String que representa al valor que tiene el modo de direccionamiento Directo
   * @return String
   */
  override def getValorString() : String = inmediato.getValorString
  
  /**
   * Devuelve la representacion en String del modo de direccionamiento Directo:
   * [XXXX] donde XXXX es un valor hexadecimal.
   * @return W16
   */
  def representacionString() :String = "[" + inmediato.representacionString + "]"
  
  /**
   * Devuelve el codigo de modo del modo de direccionamiento: 
   * @return String
   */ 
  def codigo() :String  = "001000"
 // override def bits() : Int = this.toBinaryString.size - 1
}

case class Indirecto (directo : Directo) extends ModoDireccionamiento{
  
  /**
   * Devuelve el valor que tiene el modo de direccionamiento Indirecto
   * @return W16
   */
  def getValor() : W16 = directo.getValor
  
  /**
   * Devuelve el String que representa al valor que tiene el modo de direccionamiento Indirecto
   * @return String
   */
  override def getValorString() : String = directo.getValorString
  
  /**
   * Devuelve la representacion en String del modo de direccionamiento Indirecto:
   * [[XXXX]] donde XXXX es un valor hexadecimal.
   * @return W16
   */
  def representacionString() :String = "[" + directo.representacionString + "]"
  
  /**
   * Devuelve el codigo del modo de direccionamiento: 
   * @return String
   */ 
  def codigo() :String  = "011000"
 // override def bits() : Int = this.toBinaryString.size - 1
}

case class Etiqueta (etiqueta : String) extends ModoDireccionamiento {
  
  def getValor() : W16 = null
  override def getValorString() : String = ""
  def representacionString() :String = etiqueta
  def codigo() :String  = "001000"
}
