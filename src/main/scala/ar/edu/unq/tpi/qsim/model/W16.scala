package ar.edu.unq.tpi.qsim.model

import ar.edu.unq.tpi.qsim.utils.Util

class W16(var hex: String) {
  
  var signo: Int = 0
  
  /**
   * Devuelve el valor hexadecimal que contiene
   * @return String
   */
  override def toString() =  hex
  
  /**
   * Devuelve un boleano indicando si el W16 receptor de este mensaje
   * es igual al W16 enviado por parametro comparando sus valores.
   * @parameters w16:W16
   * @return Boolean
   */
  def equals(w16:W16): Boolean =  this.value == w16.value
  
  /**
   * Devuelve el valor entero del hexadecimal que contiene.
   * @return Int
   */
  def value : Int = Util.toInteger(hex)
  
   /**
   * Devuelve la representacion en binario del hexadecimal que contiene.
   * @return Int
   */
  def toBinary: String = Util.hexToBinary(hex)
  
  /**
   * Aumenta el hexadecimal que contiene en uno.
   * 
   */
  def ++ = hex = Util.toHex4(this.value + 1)
  
 /**
   * Aumenta el hexadecimal que contiene en el valor 
   * que contenga el salto pasado por parametro.
   * @parameters salto:Int
   */
  def ++(salto:Int) = hex= Util.toHex4(this.value + salto)
  
 /**
   * Decrementa el hexadecimal que contiene en uno.
   * 
   */
  def -- = hex= Util.toHex4(this.value -1)
  
 /**
   * Cambia valor hexadecimal que contiene por la representacion  
   * del valor hexadecimal del entero pasado por parametro.
   * @parameters value: Int
   */ 
  def this(value: Int) =   this(Util.toHex4(value))
  
  /**
   * Devuelve un nuevo W16 con el valor anterio mas el valor del 
   * salto recibido.
   * @parameters salto: Int 
   * @return W16
   */
  def ss(salto:Int) : W16 = new W16(Util.toHex4(this.value + salto))

 /**
   * Le asigna un nuevo valor hexadecimal recibido por parametro.
   * @parameters w16 : String 
   */
  def :=(w16 : String) = hex = w16
  
 /**
   * Devuelve el resultado de la suma entre los dos W16.
   * @parameters w16 : W16
   * @return W16
   */
  def +(w16 : W16) :W16 = {
   val result_value = Util.toHex(this.value+w16.value)
   new W16(result_value)
  }
  
  /**
   * Devuelve el resultado de la resta entre los dos W16.
   * @parameters w16 : W16
   * @return W16
   */
  def -(w16 : W16) :W16 = {
    
   val result_value = Util.toHex(this.value-w16.value)
   new W16(result_value)
  }
  
 /**
   * Devuelve el resultado de la multiplicacion entre los dos W16.
   * @parameters w16 : W16
   * @return W16
   */
  def *(w16 : W16) :W16 = {
    
   val result_value = Util.toHex(this.value*w16.value)
   new W16(result_value)
  }
  
 /**
   * Devuelve el resultado de la division entre los dos W16.
   * @parameters w16 : W16
   * @return W16
   */
  def /(w16 : W16) :W16 = {
    
   val result_value = Util.toHex(this.value/w16.value)
   new W16(result_value)
  }
  
}

object prueba extends App() {

 /* var w = new W16("0001")
  var w2 = new W16("0010")
  
 
  println(w2-w2)
  println(w2-w)
  w.operacion_matematica(_+_, 1, 2)*/
  val d = "0001011".substring(0, 4)
  println(d)
   println("0001011".replace(d, ""))
  
}
  

