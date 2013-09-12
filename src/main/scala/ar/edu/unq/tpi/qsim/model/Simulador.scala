package ar.edu.unq.tpi.qsim.model

import ar.edu.unq.tip.qsim.state._

import scala.collection.mutable.ArrayBuffer
import ar.edu.unq.tip.qsim.state.Inicial


case class Simulador() {

  var cpu: CPU = _
  var memoria: Memoria = _
  var programaActual : Programa = _
  var instruccionActual: Instruccion = _

  def inicializarSim() {
    println("--------INIT------")
    cpu = CPU()
    memoria = Memoria(25)
    memoria.initialize()
  }

  def cargarProgramaYRegistros(programa: Programa, pc: String, registros: Map[String,W16]) {
    programaActual = programa
    cpu.cargarPc(pc) 
    cpu.actualizarRegistros(registros)
    memoria.cargarPrograma(programaActual, pc)
    println("Ver el programa cargado en Memoria: \n" + memoria.show(pc))
    
  }

  def ejecucion(){
   println("Empezando con la Ejecucion")
   do {
	   fetch()
	   decode()
	   execute()
    } while (!(programaActual.finalizo))
   
   println("Finalizo la ejecucion") 
  }
  
  def buscarInstruccion(): Instruccion = 
  {
	 programaActual.obtenerInstruccion()
  }
 	
  def fetch(){
     println("----------FETCH ---------")
	 println("Valor del Pc: " + cpu.pc.toString())
	 instruccionActual = buscarInstruccion()  
	 val instruccion_fech = instruccionActual.representacionHexadecimal()
	 println("------Trajo la instruccion a Ejecutar que apunta pc :" + instruccion_fech)
	 cpu.ir = instruccion_fech  
	 cpu.incrementarPc(instruccionActual.cantidadCeldas())
	 println("Cual es el valor de Pc luego del Fetch: " + cpu.pc)
  } 
  
  def decode() : String =
  {  println("----------DECODE------------")
     println("Que decodifico : " + instruccionActual.toString )
     instruccionActual.toString    
  }

  def obtenerValor(modoDir : ModoDireccionamiento) : W16= modoDir match {
  	 case Directo(inmediato:Inmediato) => memoria.getValor(inmediato.getValorString())
  	 case _ => modoDir.getValor
  }
  
  def execute() {
    println("-------------EXECUTE---------")
	 instruccionActual match {
	 case ADD(op1,op2) => cpu.alu.execute_add(op1.getValor,op2.getValor)
	 case MUL(op1,op2) => cpu.alu.execute_mul(op1.getValor,op2.getValor)
	 case DIV(op1,op2) => cpu.alu.execute_div(op1.getValor,op2.getValor)
	 case SUB(op1,op2) => cpu.alu.execute_sub(op1.getValor,op2.getValor)
     case MOV(op1:Registro,op2) => op1.setValor(obtenerValor(op2))
	 }   
    println("Ejecuta la instruccion!!!")
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