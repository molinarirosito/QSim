package ar.edu.unq.tpi.qsim.model

import scala.collection.mutable.Buffer
import scala.collection.mutable.Map

object Programa {

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

  def tamanioDelPrograma(): Int = (instrucciones.map(i => i.tamanioHex).fold(0)(_ + _)) / 4

  def actualizarIndice() {
    indice = indice + 1
  }

  def obtenerInstruccion(): Instruccion = {
    var siguienteInstruccion = instrucciones(indice)
    actualizarIndice()
    siguienteInstruccion
  }

  def finalizo(): Boolean = {
    indice == (instrucciones.length)
  }
  
  override def toString() = {
    s"Program(instrucciones: $instrucciones, etiquetas: $etiquetas)"
  }

}

object dd extends App {
  print(List(ADD(R1, Inmediato(new W16("0013"))), ADD(R1, Inmediato(new W16("0013")))))
}
