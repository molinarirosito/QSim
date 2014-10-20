package ar.edu.unq.tpi.qsim.utils

/**
* Copyright 2014 Tatiana Molinari.
* Copyright 2014 Susana Rosito
*
* This program is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with this program. If not, see <http://www.gnu.org/licenses/>.
*
*/

import java.lang.Byte

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
  def toInteger(chain: String): Integer =
    {
      Integer.parseInt(chain, 16)
    }
  def fromBinaryToHex4(chain: String): String = {
    var chainInBinary = chain.takeRight(16)
    var interpretacionHex = Integer.parseInt(chainInBinary, 2)
    toHex4(interpretacionHex)
  }
  /**
   * Representa un numero en una cadena binaria de 3 bit.
   *
   * @param number: Int
   * @return String
   */
  def toBinary3B(number: Int): String =
    {
      to(Integer.toBinaryString(number), 2)
    }

  /**
   * Representa un numero en una cadena binaria de 4 bit.
   *
   * @param number: Int
   * @return String
   */
  def toBinary4B(number: Int): String =
    {
      to(Integer.toBinaryString(number), 3)
    }

  /**
   * Representa un numero en una cadena binaria de 16 bit.
   *
   * @param number: Int
   * @return String
   */
  def toBinary16B(number: Int): String =
    {
      to(Integer.toBinaryString(number), 15)
    }

  /**
   * Representa un numero en una cadena binaria de 8 bit.
   *
   * @param number: Int
   * @return String
   */
  def toBinary8B(number: Int): String =
    {
      to(Integer.toBinaryString(number), 7)
    }

  /**
   * Representa un numero en una cadena binaria de 16 bit mas uno de overflow.
   *
   * @param number: Int
   * @return String
   */
  def toBinary16BOverflow(number: Int): String =
    {
      to(Integer.toBinaryString(number), 16).takeRight(17)
    }

  def toBinary32B(number: Int): (String, String) =
    {
      var binary32 = to(Integer.toBinaryString(number), 32).takeRight(32)
      var bitsMenosSignificativos = binary32.takeRight(16)
      var bitsMasSignificativos = binary32.take(16)
      (bitsMasSignificativos, bitsMenosSignificativos)
    }

  /**
   * Representa un numero dependiendo de la funcion que se le pasa por parametro.
   * functions: toHex o toBinary
   */
  def to(represent: ⇒ String, countBit: Int): String =
    {
      var new_number = represent
      var new_string = ""

      for (x ← new_number.size to countBit) {
        new_string = new_string + "0"
      }
      new_string + new_number
    }
  /**
   * Interpreta una cadena hexadecimal para saber su valor.
   * @param number: String
   * @return Integer
   */
  def hexToInteger(number: String): Integer =
    {
      Integer.parseInt(number, 16)
    }

  /**
   * Interpreta una cadena binaria para saber su valor.
   * @param number: String
   * @return Integer
   */
  def binaryToInteger(number: String): Integer =
    {
      Integer.parseInt(number, 2)
    }

  /**
   * Transforma una cadena binaria en una cadena hexadecimal.
   * @param number: String
   * @return String
   */
  def binaryToHex(binaryNumber: String): String =
    {
      val value = binaryToInteger(binaryNumber)
      toHex(value)
    }

  /**
   * Transforma una cadena binaria de 16 bits en una cadena hexadecimal de 4 bits.
   * @param number: String
   * @return String
   */
  def binary16ToHex(binaryChain: String): String =
    {
      var chain_hex = ""
      var chain_binary = binaryChain
      for (x ← 0 to 3) {
        // saque el espacio entre los numeros 
        chain_hex = binaryToHex(chain_binary.takeRight(4)) + chain_hex
        chain_binary = chain_binary.dropRight(4)
      }
      chain_hex.trim
    }

  /**
   * Transforma una cadena hexadecimal en una cadena binaria de 16 bits.
   * @param number: String
   * @return String
   */
  def hexToBinary(hexNumber: String): String =
    {
      val value = hexToInteger(hexNumber)
      toBinary16B(value)
    }

  /**
   * Convertir numero en CA2 de 8 bits.
   * @param number: Int
   * @return String
   */
  def intToCa2_8B(number: Int) = {
    var cadena = ""
    var invertirsigno = 0
    if (number < 0) {
      invertirsigno = (-1) * number
      cadena = representarNumeroEnCA2(invertirsigno)
    } else {
      cadena = toBinary8B(number)
    }
    cadena
  }

  def representarNumeroEnCA2(number: Int) = {
    var cadena = toBinary8B(number)
    var cadena_invertida = aplicarNot(cadena)
    toBinary8B(incrementarEn1(cadena_invertida))
  }

  def aplicarNot(chain: String) = {
    var cadena = chain
    var cadenaresultante = ""
    cadena.foreach(bit ⇒ {
      bit match {
        case '0' ⇒ cadenaresultante += '1'
        case '1' ⇒ cadenaresultante += '0'
      }
    })
    cadenaresultante
  }

  def incrementarEn1(chain: String) = {
    binaryToInteger(chain) + 1
  }
  /**
   * Transforma una cadena hexadecimal en una cadena binaria de 4 bits.
   * @param number: String
   * @return String
   */
  def hexTo4Binary(hexNumber: String): String =
    {
      val value = hexToInteger(hexNumber)
      toBinary4B(value)
    }

  def rep[A](n: Int)(f: ⇒ A) { if (n > 0) { f; rep(n - 1)(f) } }

  def IntSumToHex(number: Int): String = toHex(number)

}

object Testha extends App {

  val t = Util.binaryToInteger(Util.intToCa2_8B(-6))
  println(t)
  //val a = Util.toHex4(1)
  //println(a)

}
