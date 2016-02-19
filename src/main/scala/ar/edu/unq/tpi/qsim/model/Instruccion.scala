package ar.edu.unq.tpi.qsim.model
import ar.edu.unq.tpi.qsim.utils._

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

abstract class Instruccion(val codigoDeOperacion: String, val operacion: String) {
  var position: W16 = null

  /**
   * Devuelve la posicion en memoria
   * @return W16
   */
  def posicionEnMemoria(): W16 = position

  def representacionHexadecimal(): String

  /**
   * Devuelve la simulacion del decode
   * @return String
   */
  def decode(): String = this.toString

  /**
   * Devuelve el tamanio que ocupa la instruccion en tamanio hexadecimal
   * @return Int
   */
  def tamanioHex(): Int = this.representacionHexadecimal.replace(" ", "").size

  /**
   * Devuelve la cantidad de celdas que ocupa la instruccion en tamanio hexadecimal
   * @return Int
   */
  def cantidadCeldas(): Int = this.tamanioHex / 4

}

/** INSTRUCCIONES SIN OPERANDOS **/
class Instruccion_SinOperandos(codigoDeOperacion: String, operacion: String, val relleno: String) extends Instruccion(codigoDeOperacion, operacion) {

  /**
   * Redefine la representacion hexadecimal para obtener la correspondiente a
   * una instruccion sin operandos devolviendo un String hexadecimal
   * @return String
   */
  override def representacionHexadecimal(): String = {
    val operations_code_binary = Util.binary16ToHex(codigoDeOperacion + relleno)
    (operations_code_binary).replace("  ", " ")
  }

  /**
   * Redefine la representacion Hexadecimal para mostrar la instruccion
   * de la siguiente manera:  *OPERACION*
   * @return String
   */
  override def toString() = operacion
}

case class RET() extends Instruccion_SinOperandos("1100", "RET", "000000000000") {}

/** INSTRUCCIONES CON UN OPERANDO **/
abstract class Instruccion_UnOperando(codigoDeOperacion: String, operacion: String, var operando: ModoDireccionamiento, val relleno: String) extends Instruccion(codigoDeOperacion, operacion) {

  /**
   * Redefine la representacion en String para mostrar la instruccion
   * de la siguiente manera:  *OPERACION* *MODODIRECCIONAMIENTO*
   * @return String
   */
  override def toString() = operacion + " " + operando.toString()
}

/** INSTRUCCIONES CON UN OPERANDO ORIGEN**/
class Instruccion_UnOperando_Origen(codigoDeOperacion: String, operacion: String, oper: ModoDireccionamiento, relleno: String) extends Instruccion_UnOperando(codigoDeOperacion, operacion, oper, relleno) {
  /**
   * Redefine la representacion hexadecimal para obtener la correspondiente a
   * una instruccion de un operando origen devolviendo un String hexadecimal
   * @return String
   */
  override def representacionHexadecimal(): String = {
    val operations_code_binary = Util.binary16ToHex(codigoDeOperacion + relleno + operando.codigo)
    val origen_value = operando.getValorString
    (operations_code_binary + " " + origen_value).replace("  ", " ")
  }

}

case class CALL(orig: ModoDireccionamiento) extends Instruccion_UnOperando_Origen("1011", "CALL", orig, "000000") {}
case class PUSH(orig: ModoDireccionamiento) extends Instruccion_UnOperando_Origen("1110", "PUSH", orig, "000000") {}
case class JMP(orig: ModoDireccionamiento) extends Instruccion_UnOperando_Origen("1010", "JMP", orig, "000000") {}

/** INSTRUCCIONES CON UN DESTINO **/
class Instruccion_UnOperando_Destino(codigoDeOperacion: String, operacion: String, operando: ModoDireccionamiento, relleno: String) extends Instruccion_UnOperando(codigoDeOperacion, operacion, operando, relleno) {
  /**
   * Redefine la representacion hexadecimal para obtener la correspondiente a
   * una instruccion de un operando destino devolviendo un String hexadecimal
   * @return String
   */
  override def representacionHexadecimal(): String = {
    val operations_code_binary = Util.binary16ToHex(codigoDeOperacion + operando.codigo + relleno)
    val origen_value = operando.getValorString
    (operations_code_binary + " " + origen_value).replace("  ", " ")
  }

}

case class NOT(orig: ModoDireccionamiento) extends Instruccion_UnOperando_Destino("1001", "NOT", orig, "000000") {}
case class POP(orig: ModoDireccionamiento) extends Instruccion_UnOperando_Destino("1101", "POP", orig, "000000") {}

/** INSTRUCCIONES CON DOS OPERANDOS **/
class Instruccion_DosOperandos(codigoDeOperacion: String, operacion: String, var destino: ModoDireccionamiento, var origen: ModoDireccionamiento) extends Instruccion(codigoDeOperacion, operacion) {
  /**
   * Redefine la representacion hexadecimal para obtener la correspondiente a
   * una instruccion de dos operando devolviendo un String hexadecimal
   * @return String
   */
  override def representacionHexadecimal(): String = {
    val operations_code_binary = Util.binary16ToHex(codigoDeOperacion + destino.codigo + origen.codigo)
    val destiny_value = destino.getValorString
    val origin_value = origen.getValorString
    (operations_code_binary + " " + destiny_value + " " + origin_value).replace("  ", " ")
  }

  /**
   * Redefine la representacion en String para mostrar la instruccion
   * de la siguiente manera:  *OPERACION* *MODODIRECCIONAMIENTO DESTINO* *MODODIRECCIONAMIENTO ORIGEN*
   * @return String
   */
  override def toString() = operacion + " " + destino.toString() + ", " + origen.toString()
}

case class MUL(dest: ModoDireccionamiento, orig: ModoDireccionamiento) extends Instruccion_DosOperandos("0000", "MUL", dest, orig) {}
case class ADD(dest: ModoDireccionamiento, orig: ModoDireccionamiento) extends Instruccion_DosOperandos("0010", "ADD", dest, orig) {}
case class SUB(dest: ModoDireccionamiento, orig: ModoDireccionamiento) extends Instruccion_DosOperandos("0011", "SUB", dest, orig) {}
case class DIV(dest: ModoDireccionamiento, orig: ModoDireccionamiento) extends Instruccion_DosOperandos("0111", "DIV", dest, orig) {}
case class MOV(dest: ModoDireccionamiento, orig: ModoDireccionamiento) extends Instruccion_DosOperandos("0001", "MOV", dest, orig) {}
case class AND(dest: ModoDireccionamiento, orig: ModoDireccionamiento) extends Instruccion_DosOperandos("0100", "AND", dest, orig) {}
case class OR(dest: ModoDireccionamiento, orig: ModoDireccionamiento) extends Instruccion_DosOperandos("0101", "OR", dest, orig) {}
case class CMP(dest: ModoDireccionamiento, orig: ModoDireccionamiento) extends Instruccion_DosOperandos("0110", "CMP", dest, orig) {}

/** JUMPS CONDICIONALES **/
class JUMP_condicional(codigoDeOperacion: String, operacion: String, var desplazamiento: Salto) extends Instruccion(codigoDeOperacion, operacion) {
  val prefijo = "1111"
  var destino: ModoDireccionamiento = _

  /**
   * Redefine la representacion hexadecimal para obtener la correspondiente a
   * un salto condicional devolviendo un String hexadecimal
   * @return String
   */
  override def representacionHexadecimal(): String = {
    val operations_code_binary = Util.binary16ToHex(prefijo + codigoDeOperacion + desplazamiento.toBinary)
    (operations_code_binary).replace("  ", " ")
  }

  /**
   * Redefine la representacion en String para mostrar la instruccion
   * de la siguiente manera:  *OPERACION* *DESPLAZAMIENTO*
   * @return String
   */
  override def toString() = operacion + " " + desplazamiento.toString
}

case class JE(desp: Salto) extends JUMP_condicional("0001", "JE", desp) {}
case class JNE(desp: Salto) extends JUMP_condicional("1001", "JNE", desp) {}
case class JLE(desp: Salto) extends JUMP_condicional("0010", "JLE", desp) {}
case class JG(desp: Salto) extends JUMP_condicional("1010", "JG", desp) {}
case class JL(desp: Salto) extends JUMP_condicional("0011", "JL", desp) {}
case class JGE(desp: Salto) extends JUMP_condicional("1011", "JGE", desp) {}
case class JLEU(desp: Salto) extends JUMP_condicional("0100", "JLEU", desp) {}
case class JGU(desp: Salto) extends JUMP_condicional("1100", "JGU", desp) {}
case class JCS(desp: Salto) extends JUMP_condicional("0101", "JCS", desp) {}
case class JNEG(desp: Salto) extends JUMP_condicional("0110", "JNEG", desp) {}
case class JVS(desp: Salto) extends JUMP_condicional("0111", "JVS", desp) {}

object Testuds extends App {

  //instrucciones sin operandos
  //val ret = RET().representacionHexadecimal
  //var string_split = Util.hexToBinary(ret)
  //println(string_split)

  //println(Util.hexToBinary(ret))

  //instruccione modo origen
  /*  val call=CALL(Inmediato("000F")).representacionHexadecimal
    val jmp=JMP(Inmediato("000F")).representacionHexadecimal
    
    //instruccione modo destino
    val not=NOT(R4).representacionHexadecimal
    
    //instrucciones dos operandos
    val mul = MUL(R4, Inmediato("F0F0")).representacionHexadecimal
    val add = ADD( Inmediato("F0F0"), Directo(Inmediato("FFFF"))).representacionHexadecimal
    val div = DIV(Directo(Inmediato("0001")), Indirecto(Directo(Inmediato("0002")))).representacionHexadecimal
    val sub = SUB(Indirecto(Directo(Inmediato("0003"))), RegistroIndirecto(R5)).representacionHexadecimal
    val mov = MOV(RegistroIndirecto(R5), R4).representacionHexadecimal
    val and = AND(R4, Inmediato("0004")).representacionHexadecimal
    val or = OR(R4, Inmediato("0005")).representacionHexadecimal
    val cmp = CMP(R4, Inmediato("0006")).representacionHexadecimal
    
    // jmps condicionales
    
    val je =  JE(new Salto(0)).representacionHexadecimal
	val jne = JNE(new Salto(1)).representacionHexadecimal
	val jle = JLE(new Salto(2)).representacionHexadecimal
	val jg = JG(new Salto(3)).representacionHexadecimal 
	val jl = JL(new Salto(4)).representacionHexadecimal 
	val jge = JGE(new Salto(5)).representacionHexadecimal 
	val jleu = JLEU(new Salto(6)).representacionHexadecimal 
	val jgu = JGU(new Salto(7)).representacionHexadecimal 
	val jcs = JCS(new Salto(8)).representacionHexadecimal 
	val jneg = JNEG(new Salto(0)).representacionHexadecimal
	val jvs = JVS(new Salto(0)).representacionHexadecimal */
}
