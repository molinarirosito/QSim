package ar.edu.unq.tpi.qsim.model

import ar.edu.unq.tip.qsim.state._
import ar.edu.unq.tpi.qsim.exeptions._

import scala.collection.mutable.ArrayBuffer
import ar.edu.unq.tip.qsim.state.Inicial
import ar.edu.unq.tpi.qsim.utils._

case class Simulador() {

  var cpu: CPU = _
  var memoria: Memoria = _
  var instruccionActual: Instruccion = _

  def inicializarSim() {
    println("--------INIT------")
    cpu = CPU()
    memoria = Memoria(30)
    memoria.initialize()
  }

  def cargarProgramaYRegistros(programa: Programa, pc: String, registros: Map[String, W16]) {
    cpu.cargarPc(pc)
    cpu.actualizarRegistros(registros)
    memoria.cargarPrograma(programa, pc)
    println("Ver el programa cargado en Memoria: \n" + memoria.show(pc))

  }

  def ejecucion(programa: Programa) {
    var n = 1
    println("Empezando con la Ejecucion") 
    do {
      fetch()
      decode()
      execute()
      n=n+1
    } while (n<=programa.instrucciones.size)
    println("Finalizo la ejecucion")
  }


  def obtenerProximaInstruccionBinario() : String =
  {
    val int_pc = cpu.pc.value
    memoria.getValor(int_pc).toBinary + memoria.getValor(int_pc+1).toBinary + memoria.getValor(int_pc+2).toBinary    
  }
  
  def fetch() {
    println("----------FETCH ---------")
    println("Valor del Pc: " + cpu.pc.toString())
    val cadena_binaria = obtenerProximaInstruccionBinario()
    instruccionActual = Ensamblador.ensamblarInstruccion(cadena_binaria)
    val instruccion_fech = instruccionActual.representacionHexadecimal()
    println("------Trajo la instruccion a Ejecutar que apunta pc :" + instruccion_fech)
    cpu.ir = instruccion_fech
    cpu.incrementarPc(instruccionActual.cantidadCeldas())
    println("Cual es el valor de Pc luego del Fetch: " + cpu.pc)
  }

  def decode(): String =
    {
      println("----------DECODE------------")
      println("Que decodifico : " + instruccionActual.toString)
      instruccionActual.toString
    }

  def obtenerValor(modoDir: ModoDireccionamiento): W16 = modoDir match {
    case Directo(inmediato: Inmediato)=> memoria.getValor(inmediato.getValorString())
    case _ => modoDir.getValor
  }


  def execute_instruccion_matematica(): W16 = {
    println("--------INSTRUCCION PARA ALU------")
    var resultado: Map[String, Any] = null
    instruccionActual match {
      case ADD(op1, op2) => resultado = ALU.execute_add(obtenerValor(op1), obtenerValor(op2))
      case MUL(op1, op2) => resultado = ALU.execute_mul(obtenerValor(op1), obtenerValor(op2))
      case DIV(op1, op2) => resultado = ALU.execute_div(obtenerValor(op1), obtenerValor(op2))
      case SUB(op1, op2) => resultado = ALU.execute_sub(obtenerValor(op1), obtenerValor(op2))
    }
    cpu.actualizarFlags(resultado)
    resultado("resultado").asInstanceOf[W16]
  }

  def execute() {
    println("-------------EXECUTE---------")
    instruccionActual match {
      // te gusta que sea de esta forma??
      case RET() => executeRet()
      case CALL(op1) => executeCall(op1)
      case MOV(op1, op2) => store(op1, obtenerValor(op2))
      case iOp2:Instruccion_DosOperandos => store(iOp2.destino, execute_instruccion_matematica())
    }
    println("Ejecuta la instruccion!!!")
  }

  def store(modoDir: ModoDireccionamiento, un_valor: W16) = modoDir match {
    case Directo(inmediato: Inmediato) => memoria.setValor(inmediato.getValorString(), un_valor)
    case r: Registro => r.valor = un_valor
      println(s"Se guarda el resutado $un_valor en " + modoDir.toString)
  }
  
  def executeRet(){
    cpu.sp.++
    cpu.pc.:=(memoria.getValor(cpu.sp.toString).toString)
  }
  
  def executeCall(modoDir: ModoDireccionamiento){
    // guarda [SP] <- PC
    memoria.setValor(cpu.sp.toString, cpu.pc)
    // subimos el sp
    cpu.sp.--
    // guarda pc <- origen
    cpu.pc.:=(modoDir.getValor.toString)
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