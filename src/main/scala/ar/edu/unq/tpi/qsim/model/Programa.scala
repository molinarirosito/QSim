package ar.edu.unq.tpi.qsim.model

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


import scala.collection.mutable.Buffer
import scala.collection.mutable.Map

object Programa {

  /**
   * Aplica los datos pasados por una lista de que contiene opciones e instrucciones
   * relacionadas y devuelve un programa.
   * @parameters datas: List[(Option[String], Instruccion)]
   * @return Programa
   */
  def apply(datas: List[(Option[String], Instruccion)]): Programa = {
    var etiquetas = Map[String, Instruccion]()
    datas.foreach(entry => {
      entry match {
        case (Some(etiqueta), i) => etiquetas(etiqueta) = i
        case _ =>
      }
    })
    new Programa(datas.map(_._2), etiquetas)
  }
}

class Programa(var instrucciones: List[Instruccion], var etiquetas: Map[String, Instruccion] = Map()) {
  var indice = 0

  /**
   * Devuelve el tamanio del programa
   * @return Int
   */
  def tamanioDelPrograma(): Int = (instrucciones.map(i => i.tamanioHex).fold(0)(_ + _)) / 4

  /**
   * Actualiza el indice, aumentandolo en uno
   *
   */
  def actualizarIndice() {
    indice = indice + 1
  }

  /**
   * Obtiene la siguiente instruccion segun lo indique el indice y lo aumenta.
   * @return Instruccion
   */
  def obtenerInstruccion(): Instruccion = {
    var siguienteInstruccion = instrucciones(indice)
    actualizarIndice()
    siguienteInstruccion
  }

  /**
   * Retorna un booleano verificando si el programa termino.
   * @return Boolean
   */
  def finalizo(): Boolean = {
    indice == (instrucciones.length)
  }
  /**
   * Retorna si dos programas son iguales. Esto ocurre cuando tiene el mismo tamanio y las mismas instrucciones.
   */
  def equals(programa: Programa): Boolean = this.tamanioDelPrograma == programa.tamanioDelPrograma && containsSameInstructions(programa)

  def containsSameInstructions(programa: Programa): Boolean = {
    var isSame = true
    var indice = 0
    do {
      var instruccionActual = this.instrucciones(indice)
      var instruccionAComparar = programa.instrucciones(indice)
      if (!(instruccionActual.representacionHexadecimal() == instruccionAComparar.representacionHexadecimal()))
        isSame = false

      indice = indice + 1
    } while (indice < (this.instrucciones.length - 1) && isSame)
    
    isSame
  }

  /**
   * Devuelve el String que representa el programa de una manera mas amigable.
   * @return String
   */
  override def toString() = {
    s"Program(instrucciones: $instrucciones, etiquetas: $etiquetas)"
  }

}

object dd extends App {
  
 var inst = JE(new Salto(1))
 println(inst.representacionHexadecimal)
 println(inst.toString)
 
  //print(List(ADD(R1, Inmediato(new W16("0013"))), ADD(R1, Inmediato(new W16("0013")))))
 var instruccion = MOV(new Directo(new Inmediato(new W16("0005"))), new Etiqueta("etiqueta"))
 println(instruccion.representacionHexadecimal)  
}
	