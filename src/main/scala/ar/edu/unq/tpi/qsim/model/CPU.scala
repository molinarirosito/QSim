package ar.edu.unq.tpi.qsim.model

import scala.collection.mutable.ArrayBuffer
import ar.edu.unq.tpi.qsim.utils.Util
import ar.edu.unq.tip.qsim.state.Estado
import ar.edu.unq.tip.qsim.state.Inicial
import scala.collection.mutable.Map

import org.uqbar.commons.utils.Observable

@Observable
case class CPU() {

  var registros = ArrayBuffer[Registro](R0, R1, R2, R3, R4, R5, R6, R7)
  var pc = new W16("0000")
  var sp = new W16("02DF")
  var ir = ""
  var alu = ALU

  var n = 0
  var v = 0
  var z = 0
  var c = 0

  /**
   * Encuentra un registro por medio de su representacion en String
   * @params registroString: String
   */
  def registro(registroString: String) = registros.find(_.representacionString == registroString)

  /**
   * Incrementa el pc en una cantidad que se se pasa por parametro
   * @params cantidad: Int
   */
  def incrementarPc(cantidad: Int) = {
    pc.++(cantidad)
  }

  /**
   * Carga un valor hexadecimal en el pc pasado por parametro.
   * @parameters valor: String
   */
  def cargarPc(valor: String) {
    pc = new W16(valor)
  }

  /**
   * Actualiza R7 con un valor pasado en un Map.
   * @parameters registroValue: Map[String, Any]
   */
  def actualizarR7(registroValue: Map[String, Any]) {
    if (registroValue.contains("R7")) {
      registro("R7") match {
        case Some(reg) => reg.setValor(registroValue("R7").asInstanceOf[W16])
        case None =>
      }
    }
  }

  /**
   * Actualiza los registros por medio de un Map que recibe por parametro.
   * @parameters registrosAct: Map[String, W16]
   */
  def actualizarRegistros(registrosAct: Map[String, W16]) {
    for (x <- 0 to 7) {
      if (registrosAct.contains(s"R$x")) {
        this.registros(x).setValor(registrosAct(s"R$x"))
      }
    }
  }

  /**
   * Actualiza los flags por medio de un Map que recibe por parametro.
   * @parameters flags: Map[String, Any]
   */
  def actualizarFlags(flags: Map[String, Any]) {
    v = flags("v").asInstanceOf[Int]
    z = flags("z").asInstanceOf[Int]
    c = flags("c").asInstanceOf[Int]
    n = flags("n").asInstanceOf[Int]
  }
}

object pur extends App() {

  var cpu = CPU()
  print(cpu.registros + "\n")
  print(cpu.pc)
  
}