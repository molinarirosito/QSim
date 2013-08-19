package ar.edu.unq.tpi.qsim.model

import scala.collection.mutable.ArrayBuffer
import ar.edu.unq.tpi.qsim.utils.Util
import ar.edu.unq.tip.qsim.state.Estado
import ar.edu.unq.tip.qsim.state.Inicial

case class CPU() {

  var registros = ArrayBuffer[Registro](R0, R1, R2, R3, R4, R5, R6, R7)
  var n = 0
  var v = 0
  var z = 0
  var c = 0
  var pc = Util.toHex(0)
  var ir = ""
  var alu = new ALU()
  var uc = new UnidadControl(this)
  var estado: Estado = _
  var irDecode = ""

  def incrementarPc() {
    pc = Util.toHex(Util.hexToInteger(pc) + 1)
  }

  def cambiarEstado(estadoACambiar: Estado) {
    estado = estadoACambiar
  }
  def cargarPc(valor: String) {
    pc = valor
  }

  def ejecutarPrograma(programa: Programa) {
    var instrucciones = programa.instrucciones
    instrucciones.foreach(instruccion => uc.ejecutarCicloInstruccion(instruccion))		
    
  }

  def inicializar(registros: Map[String, Any]) {
    cambiarEstado(new Inicial(this))
    estado.actualizarRegistros(registros)
  }

}

object pur extends App() {

  var cpu = CPU()
  print(cpu.registros + "\n")
  print(cpu.pc)
}