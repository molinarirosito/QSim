package ar.edu.unq.tpi.qsim.model

import ar.edu.unq.tip.qsim.state.Estado
import ar.edu.unq.tip.qsim.state.Carga
import scala.collection.mutable.ArrayBuffer
import ar.edu.unq.tip.qsim.state.Inicial
import ar.edu.unq.tip.qsim.state.Carga
import ar.edu.unq.tip.qsim.state.Carga
import ar.edu.unq.tip.qsim.state.Ejecucion

case class Simulador(var programa: Programa) {

  var cpu: CPU = _
  var memoria: Memoria = _

  def inicializarSim() {
    cpu = CPU()
    memoria = Memoria(65536)
    memoria.initialize()
  }

  def cargarProgramaYRegistros(pc: String, registros: Map[String,Any]) {
    cpu.cargarPc(pc) // cuando estuve pensando esta situacion vi si la cpu tendria que conocer a la memoria porque en la ejecucion
    cpu.inicializar(registros)// de un programa interactuan bastante. Es que estoy pasando el pc  a los dos y la cpu podria darle a la memoria el pc se entiende. ??
    memoria.cargarPrograma(programa, pc)
    println(memoria.celdas)
  }

  def ejcutarPrograma() {
    cpu.ejecutarPrograma(programa)
  }
}

object tt extends App {
  var array = ArrayBuffer[Instruccion]()
  array += (SUB(R1, R4))
  var programa = Programa(array)
  var sim = Simulador(programa)
  sim.inicializarSim()
 // sim.cargarPrograma("0003")
}