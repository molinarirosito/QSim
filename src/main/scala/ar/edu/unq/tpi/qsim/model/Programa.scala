package ar.edu.unq.tpi.qsim.model

import scala.collection.mutable.Buffer
import scala.collection.mutable.Map

object Programa {

  /**
   * Aplica los datos pasados por una lista de que contiene opcciones e instrucciones 
   * relacionadas y devuelve un programa.
   * @parameters datas: List[(Option[String], Instruccion)]
   * @return Programa
   */
  def apply(datas: List[(Option[String], Instruccion)]):Programa = {
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
   * Devuelve el tamaño del programa
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
   * Devuelve el String que representa el programa de una manera mas amigable.
   * @return String
   */
  override def toString() = {
    s"Program(instrucciones: $instrucciones, etiquetas: $etiquetas)"
  }

}

object dd extends App {
  print(List(ADD(R1, Inmediato(new W16("0013"))), ADD(R1, Inmediato(new W16("0013")))))
}
