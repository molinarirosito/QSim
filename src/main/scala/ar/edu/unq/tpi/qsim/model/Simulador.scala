package ar.edu.unq.tpi.qsim.model

import ar.edu.unq.tip.qsim.state._
import ar.edu.unq.tpi.qsim.exeptions._
import scala.collection.mutable.Map

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

  def etiquetasInvalidas(programa: Programa): Boolean = {
    programa.instrucciones.exists(instr ⇒ instr match {
      case inst_up: Instruccion_UnOperando ⇒ (!programa.etiquetas.contains(inst_up.origen.representacionString()))
      case _ ⇒ false
    })
  }

  def asignarPosiciones(pc: W16, programa: Programa): Programa = {
    var pcAsignar: W16 = pc
    programa.instrucciones.foreach(inst ⇒ {
      inst.position = pcAsignar
      println(inst.position)
      pcAsignar = pcAsignar.ss(inst.cantidadCeldas())
    })
    programa
  }

  def calcularEtiquetas(programa: Programa): Programa = {
    programa.instrucciones.foreach(inst ⇒ {
      inst match {
        case inst_up: Instruccion_UnOperando ⇒ inst_up.origen = new Inmediato(programa.etiquetas(inst_up.origen.representacionString).position)
        case _ ⇒
      }
    })
    programa
  }

  def cargarProgramaYRegistros(programa: Programa, pc: String, registros: Map[String, W16]) {
    var pcInicial = new W16(pc)
    cpu.cargarPc(pc)
    cpu.actualizarRegistros(registros)

    if (!(etiquetasInvalidas(programa))) {
      var programaConPosiciones = asignarPosiciones(pcInicial, programa)
      var programaSinEtiquetas = calcularEtiquetas(programa)

      memoria.cargarPrograma(programaSinEtiquetas, pc)
      println("Ver el programa cargado en Memoria: \n" + memoria.show(pc))
    } else {
      println("ERROR ------- ETIQUETAS INVALIDAS -----NO SE CARGA EN MEMORIA!! ")
    }
    //    memoria.cargarPrograma(programa, pc)
  }

  def ejecucion(programa: Programa) {
    var n = 1
    println("Empezando con la Ejecucion")
    do {
      fetch()
      decode()
      execute()
      n = n + 1
    } while (n <= programa.instrucciones.size)
    println("Finalizo la ejecucion")
  }

  def obtenerProximaInstruccionBinario(): String =
    {
      val int_pc = cpu.pc.value
      memoria.getValor(int_pc).toBinary + memoria.getValor(int_pc + 1).toBinary + memoria.getValor(int_pc + 2).toBinary
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
    case Directo(inmediato: Inmediato) ⇒ memoria.getValor(inmediato.getValorString())
    case Indirecto(directo: Directo) ⇒ memoria.getValor(obtenerValor(directo))
    case RegistroIndirecto(registro: Registro) ⇒ memoria.getValor(obtenerValor(registro))
    case _ ⇒ modoDir.getValor
  }

  def execute_instruccion_matematica(): W16 = {
    println("--------INSTRUCCION PARA ALU------")
    var resultado = Map[String, Any]()
    instruccionActual match {
      case ADD(op1, op2) ⇒ resultado = ALU.execute_add(obtenerValor(op1), obtenerValor(op2))
      case MUL(op1, op2) ⇒ {
        var result_mult = ALU.execute_mul(obtenerValor(op1), obtenerValor(op2))
        cpu.actualizarR7(result_mult)
        result_mult
      }
      case DIV(op1, op2) ⇒ resultado = ALU.execute_div(obtenerValor(op1), obtenerValor(op2))
      case SUB(op1, op2) ⇒ resultado = ALU.execute_sub(obtenerValor(op1), obtenerValor(op2))
    }
    cpu.actualizarFlags(resultado)
    resultado("resultado").asInstanceOf[W16]
  }

  def execute() {
    println("-------------EXECUTE---------")
    instruccionActual match {
      // te gusta que sea de esta forma??
      case RET() ⇒ executeRet()
      case CALL(op1) ⇒ executeCall(op1)
      case MOV(op1, op2) ⇒ store(op1, obtenerValor(op2))
      //case JMP
      // case Jxx
      case CMP(op1, op2) ⇒ executeCmp(obtenerValor(op1),obtenerValor(op2))
      case iOp2: Instruccion_DosOperandos ⇒ store(iOp2.destino, execute_instruccion_matematica())
    }
    println("Ejecuta la instruccion!!!")
  }

  def store(modoDir: ModoDireccionamiento, un_valor: W16) = modoDir match {
    case Directo(inmediato: Inmediato) ⇒ memoria.setValor(inmediato.getValorString(), un_valor)
    case Indirecto(directo: Directo) ⇒ memoria.setValor(obtenerValor(directo).hex, un_valor)
    case RegistroIndirecto(registro: Registro) ⇒ memoria.setValor(obtenerValor(registro).hex, un_valor)
    case r: Registro ⇒
      r.valor = un_valor
      println(s"Se guarda el resutado $un_valor en " + modoDir.toString)
  }

  def executeRet() {
    cpu.sp.++
    cpu.pc.:=(memoria.getValor(cpu.sp.toString).toString)
  }
  def executeCmp(op1: W16, op2: W16) {
    var resultados = ALU.execute_cmp(op1, op2)
    cpu.actualizarFlags(resultados)
  }

  def executeCall(modoDir: ModoDireccionamiento) {
    // guarda [SP] <- PC
    memoria.setValor(cpu.sp.toString, cpu.pc)
    // subimos el sp
    cpu.sp.--
    // guarda pc <- origen
    cpu.pc.:=(modoDir.getValor.toString)
  }

}

object tt extends App {
  var programa = new Programa(List(SUB(R1, R4)))
  //  var sim = Simulador(programa)
  // sim.inicializarSim()
  // sim.cargarPrograma("0003")
}