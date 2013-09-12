package ar.edu.unq.tpi.qsim.utils

object Util {
  	
  /**
   * Representa un numero en una cadena hexacimal de 4 digitos.
   *
   * @param number: Int
   * @return String
   */
  def toHex4(number: Int): String = to(Integer.toHexString(number), 3).toUpperCase()
  
  /**
   * Representa un numero en una cadena hexacimal.
   *
   * @param number: Int
   * @return String
   */
  def toHex(number: Int): String = to(Integer.toHexString(number), 0).toUpperCase()

  /**
   * Interpreta una cadena hexadecimal en numero.
   * @param chain: String
   * @return Integer
   */
  def toInteger(chain:String): Integer =
  {
    Integer.parseInt(chain,16)
  }
  
  /**
   * Representa un numero en una cadena binaria de 3 bit.
   * 
   * @param number: Int
   * @return String
   */
  def toBinary3B(number:Int): String = 
  {
    to(Integer.toBinaryString(number),2)
  }
  
  /**
   * Representa un numero en una cadena binaria de 4 bit.
   * 
   * @param number: Int
   * @return String
   */
  def toBinary4B(number:Int): String = 
  {
    to(Integer.toBinaryString(number),3)
  }
  
   /**
   * Representa un numero en una cadena binaria de 16 bit.
   * 
   * @param number: Int
   * @return String
   */
  def toBinary16B(number:Int): String = 
  {
    to(Integer.toBinaryString(number),15)
  }
  
  /**
   * Representa un numero en una cadena binaria de 16 bit mas uno de overflow.
   * 
   * @param number: Int
   * @return String
   */
  def toBinary16BOverflow(number:Int): String = 
  {
    to(Integer.toBinaryString(number),16)
  }
  
  /**
   * Representa un numero dependiendo de la funcion que se le pasa por parametro.
   * functions: toHex o toBinary 
   */
  def to(represent: => String, countBit: Int): String =
  {
    var new_number = represent
    var new_string = ""
    
    for (x <- new_number.size to countBit) {
      new_string = new_string + "0"
    }
    new_string + new_number
  }
  /**
   * Interpreta una cadena hexadecimal para saber su valor.
   * @param number: String
   * @return Integer
   */
  def hexToInteger(number:String) :Integer =
  {
    Integer.parseInt(number,16)
  }
  
  /**
   * Interpreta una cadena binaria para saber su valor.
   * @param number: String
   * @return Integer
   */
  def binaryToInteger(number:String) :Integer =
  {
    Integer.parseInt(number,2)
  }
  
  /**
   * Transforma una cadena binaria en una cadena hexadecimal.
   * @param number: String
   * @return String
   */
   def binaryToHex(binaryNumber:String) :String =
   {
     val value = binaryToInteger(binaryNumber)
     toHex(value)
   }
   
   /**
   * Transforma una cadena binaria de 16 bits en una cadena hexadecimal de 4 bits.
   * @param number: String
   * @return String
   */
   def binary16ToHex(binaryChain:String) :String =
   {
     var chain_hex = ""
     var chain_binary = binaryChain
     for (x <- 0 to 3) {
      // saque el espacio entre los numeros 
      chain_hex = binaryToHex(chain_binary.takeRight(4)) + chain_hex
      chain_binary=chain_binary.dropRight(4)
    }
     chain_hex.trim
   }
   
    /**
   * Transforma una cadena hexadecimal en una cadena binaria de 16 bits.
   * @param number: String
   * @return String
   */
   def hexToBinary(binaryNumber:String) :String =
   {
     val value = hexToInteger(binaryNumber)
     toBinary16B(value)
   }



  def IntSumToHex(number: Int): String = toHex(number)
 

}

object Test extends App {

  val t = Util.binary16ToHex("0010100000000000")
  println(t)
}
