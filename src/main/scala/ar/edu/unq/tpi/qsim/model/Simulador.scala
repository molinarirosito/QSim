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

  
  def execute_instruccion_matematica() : W16 = {
    println("--------INSTRUCCION PARA ALU------")
    var resultado : Map[String,Any] = Map[String, Any](("", ""))
    instruccionActual match {
	 case ADD(op1,op2) => resultado = ALU.execute_add(obtenerValor(op1),obtenerValor(op2))
	 case MUL(op1,op2) => resultado = ALU.execute_mul(obtenerValor(op1),obtenerValor(op2))
	 case DIV(op1,op2) => resultado = ALU.execute_div(obtenerValor(op1),obtenerValor(op2))
	 case SUB(op1,op2) => resultado = ALU.execute_sub(obtenerValor(op1),obtenerValor(op2))
	 }
    cpu.actualizarFlags(resultado)
    resultado("resultado").asInstanceOf[W16]
  }
  
  def execute() {
    println("-------------EXECUTE---------")
    instruccionActual match {
     case MOV(op1:Registro,op2) => op1.setValor(obtenerValor(op2))
     case _ => execute_instruccion_matematica()
	 }   
    
    println("Ejecuta la instruccion!!!")
  }
  
  def store(modoDir :ModoDireccionamiento, un_valor : W16) = modoDir match {
  	 case Directo(inmediato:Inmediato) => memoria.setValor(inmediato.getValorString(),un_valor)
  	 case r: Registro => r.valor=un_valor
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