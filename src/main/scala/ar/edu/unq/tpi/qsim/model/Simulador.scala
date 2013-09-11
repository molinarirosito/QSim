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
    memoria = Memoria(25)
    memoria.initialize()
  }

  def cargarProgramaYRegistros(programa: Programa, pc: String, registros: Map[String,W16]) {
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
  
  
  def obtenerValor(modoDir : ModoDireccionamiento) : W16= modoDir match {
    case Directo(inmediato:Inmediato) => memoria.getValor(inmediato.getValorString)
  	case _ => modoDir.getValor
  }
  
  def execute(instruccion: Instruccion)= instruccion  match {
   
  case ADD(op1,op2) => cpu.alu.execute_add(obtenerValor(op1),obtenerValor(op1))
  case MUL(op1,op2) => cpu.alu.execute_mul(obtenerValor(op1),obtenerValor(op1))
  case DIV(op1,op2) => cpu.alu.execute_div(obtenerValor(op1),obtenerValor(op1))
  case SUB(op1,op2) => cpu.alu.execute_sub(obtenerValor(op1),obtenerValor(op1))
  case MOV(op1:Registro,op2) => op1.setValor(obtenerValor(op2))
    
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