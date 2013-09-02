package ar.edu.unq.tpi.qsim.model

import ar.edu.unq.tip.qsim.state._

import scala.collection.mutable.ArrayBuffer
import ar.edu.unq.tip.qsim.state.Inicial


case class Simulador() {

  var cpu: CPU = _
  var memoria: Memoria = _
  var programaActual : Programa = _

  def inicializarSim() {
    cpu = CPU()
    memoria = Memoria(65536)
    memoria.initialize()
  }

  def cargarProgramaYRegistros(programa: Programa, pc: String, registros: Map[String,Any]) {
    programaActual = programa
    cpu.cargarPc(pc) 
    cpu.inicializar(registros)
    memoria.cargarPrograma(programaActual, pc)
  }

  def fetch(instruccion: Instruccion){
    
	  val instruccion_fech = instruccion.representacionHexadecimal
	  cpu.incrementarPc
	  cpu.ir= instruccion_fech  
    
  }
  
  def decode(instruccion: Instruccion) : String =
  {
    instruccion.toString    
  }
  
  
  def obtenerValor(modoDir : ModoDireccionamiento) : String= modoDir match {
    case _ => modoDir.getValorString    
  }
  
  def execute(instruccion: Instruccion)= instruccion  match {
    
  case ADD(op1,op2) => cpu.alu
  case MUL(op1,op2) => cpu.alu
  case DIV(op1,op2) => cpu.alu
  case SUB(op1,op2) => cpu.alu
  case MOV(op1:Registro,op2) => op1.setValue(obtenerValor(op2))
    
  }
  
}

object tt extends App {
  var array = ArrayBuffer[Instruccion]()
  array += (SUB(R1, R4))
  var programa = Programa(array)
//  var sim = Simulador(programa)
 // sim.inicializarSim()
 // sim.cargarPrograma("0003")
}