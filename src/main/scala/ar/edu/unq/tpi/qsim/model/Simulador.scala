package ar.edu.unq.tpi.qsim.model

import ar.edu.unq.tip.qsim.state._
import ar.edu.unq.tpi.qsim.exeptions._
import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer
import ar.edu.unq.tip.qsim.state.Inicial
import ar.edu.unq.tpi.qsim.utils._
import org.uqbar.commons.utils.Observable
import org.apache.velocity.runtime.directive.Foreach

@Observable
case class Simulador() {

  private val NONE = 0
  private val PROGRAM = 1
  private val FECH_DECODE = 2
  private val STORE = 3
  private val EXECUTED = 4

  var cpu: CPU = _
  var busIO: BusEntradaSalida = _
  var instruccionActual: Instruccion = _
  var celdaInstruccionActual: ArrayBuffer[Celda] = ArrayBuffer[Celda]()

  /**
   * Inicializa el sumulador, crea la memoria y el CPU.
   */
  def inicializarSim() {
    println("--------INIT------")
    cpu = CPU()
    busIO = BusEntradaSalida()
    busIO.initialize
  }

  /**
   * Toma un programa y devuelve si tiene alguna etiqueta invalida
   * @param Programa
   * @return Boolean
   */
  def etiquetasInvalidas(programa: Programa): Boolean = {
    programa.instrucciones.exists(instr ⇒ instr match {
      case inst_dp: Instruccion_DosOperandos ⇒ verificarOperandoEtiqueta(inst_dp.origen, programa)
      case inst_up: Instruccion_UnOperando ⇒ verificarOperandoEtiqueta(inst_up.operando, programa)
      case _ ⇒ false
    })
  }
  /**
   * Verifica deacuerdo al operando que le pasan si es una etiqueta y si es invalida
   * @param Operando , Programa
   * @return Boolean
   */
  def verificarOperandoEtiqueta(operando: ModoDireccionamiento, programa: Programa): Boolean = {
    var respuesta = false
    if (operando.codigo.equals("111111")) {
      respuesta = (!programa.etiquetas.contains(operando.representacionString()))
    }

    respuesta
  }

  /**
   * Toma un pc de inicio (W16) y un programa y le asigna a cada instruccion una posicion
   * @param W16, Programa
   * @return Programa
   */
  def asignarPosiciones(pc: W16, programa: Programa): Programa = {
    var pcAsignar: W16 = pc
    programa.instrucciones.foreach(inst ⇒ {
      inst.position = pcAsignar
      pcAsignar = pcAsignar.ss(inst.cantidadCeldas())
    })
    programa
  }
   
  /**
   * Calcula de acuerdo al operando que le pasan el valor de la etiqueta
   * @param Operando , Programa
   * @return W16
   */
  def calcularValorOperandoEtiqueta(operando: ModoDireccionamiento, programa: Programa){
    var op = operando
    if (operando.codigo.equals("111111")) {
      op = new Inmediato(programa.etiquetas(op.representacionString).position) 
    }
  }

  def calcularEtiquetas(programa: Programa): Programa = {
    programa.instrucciones.foreach(inst ⇒ {
      inst match {
        case inst_dp: Instruccion_DosOperandos ⇒ calcularValorOperandoEtiqueta(inst_dp.origen, programa)
        case inst_up: Instruccion_UnOperando ⇒ calcularValorOperandoEtiqueta(inst_up.operando, programa)
        case _ ⇒
      }
    })
    programa
  }

  /**
   * Carga el programa en memoria, a partir de un pc hexadecimal (String) y los registros que recibe dentro de un map
   * @param Programa, String, Map[String,W16]
   */
  def cargarProgramaYRegistros(programa: Programa, pc: String, registros: Map[String, W16]) {
    var pcInicial = new W16(pc)
    cpu.cargarPc(pc)
    cpu.actualizarRegistros(registros)

    if (!(etiquetasInvalidas(programa))) {
      var programaConPosiciones = asignarPosiciones(pcInicial, programa)
      var programaSinEtiquetas = calcularEtiquetas(programa)

      busIO.memoria.cargarPrograma(programaSinEtiquetas, pc)
      println("Ver el programa cargado en Memoria: \n" + busIO.memoria.show(pc))
    } else {
      println("ERROR ------- ETIQUETAS INVALIDAS -----NO SE CARGA EN MEMORIA!! ")
    }
    //    memoria.cargarPrograma(programa, pc)
  }

  /**
   * Obtiene la proxima instruccion en representacion binaria. Toma tres celdas (ya que es el maximo que pueda ocupar una instruccion), si en la
   * memoria no quedan tantas, toma las que quedan nada mas. De no haber ninguna lanza una exepcion.
   * @throws CeldaFueraDeMemoriaExeption
   * @return String
   */
  def obtenerProximaInstruccionBinario(): String =
    {
      var int_pc = cpu.pc.value
      var celdas_binario = busIO.memoria.getValor(int_pc).toBinary
      try { Util.rep(2) { celdas_binario += busIO.memoria.getValor(int_pc + 1).toBinary; int_pc = int_pc + 1 } }
      catch {
        case cfme: CeldaFueraDeMemoriaException ⇒ celdas_binario
      }
      celdas_binario
    }

  /**
   * Simula el fech de la instruccion. La obtiene de memoria segun marque el pc, la ensambla y aumenta el pc.
   *
   */
  def fetch() {
    println("----------FETCH ---------")
    println("Valor del Pc: " + cpu.pc.toString())
    cambiarEstadoCeldasInstruccionActual(EXECUTED)
    val cadena_binaria = obtenerProximaInstruccionBinario()
    instruccionActual = Interprete.interpretarInstruccion(cadena_binaria)
    val instruccion_fech = instruccionActual.representacionHexadecimal()
    println("------Trajo la instruccion a Ejecutar que apunta pc :" + instruccion_fech)
    cpu.ir = instruccion_fech
    celdaInstruccionActual = obtenerCeldasInstruccionActual()
    cambiarEstadoCeldasInstruccionActual(FECH_DECODE)
    cpu.incrementarPc(instruccionActual.cantidadCeldas())
    println("Cual es el valor de Pc luego del Fetch: " + cpu.pc)
  }

  def obtenerCeldasInstruccionActual(): ArrayBuffer[Celda] =
    {
      busIO.memoria.getCeldas(cpu.pc.value, instruccionActual.cantidadCeldas())

    }

  def cambiarEstadoCeldasInstruccionActual(estado: Int) {

    celdaInstruccionActual.foreach(celda ⇒
      celda.state = estado)
  }

  /**
   * Simula el decode de la instruccion. Simplemente muestra lo que ensamblo.
   *
   */
  def decode(): String =
    {
      println("----------DECODE------------")
      println("Que decodifico : " + instruccionActual.toString)
      instruccionActual.toString
    }

  /**
   * Obtiene el valor alojado en el modo de direccionamiento que es pasado por parametro
   * @param ModoDireccionamiento
   * @return W16
   */
  def obtenerValor(modoDir: ModoDireccionamiento): W16 = modoDir match {
    case Directo(inmediato: Inmediato) ⇒ busIO.getValor(inmediato.getValorString())
    case Indirecto(directo: Directo) ⇒ busIO.getValor(obtenerValor(directo))
    case RegistroIndirecto(registro: Registro) ⇒ busIO.getValor(obtenerValor(registro))
    case _ ⇒ modoDir.getValor
  }

  /**
   * Delega en la ALU la ejecucion de una instruccion matematica. Recibe el resutlado y lo guarda en
   * el primer operando.
   *
   */
  def execute_instruccion_matematica(): W16 = {
    println("--------INSTRUCCION PARA ALU------")
    var resultado = Map[String, Any]()
    instruccionActual match {
      case ADD(op1, op2) ⇒ resultado = ALU.execute_add(obtenerValor(op1), obtenerValor(op2))
      case MUL(op1, op2) ⇒ {
        var result_mult = ALU.execute_mul(obtenerValor(op1), obtenerValor(op2))
        cpu.actualizarR7(result_mult)
        resultado = result_mult
      }
      case DIV(op1, op2) ⇒ resultado = ALU.execute_div(obtenerValor(op1), obtenerValor(op2))
      case SUB(op1, op2) ⇒ resultado = ALU.execute_sub(obtenerValor(op1), obtenerValor(op2))

    }
    cpu.actualizarFlags(resultado)
    resultado("resultado").asInstanceOf[W16]
  }

  /**
   * Simula el execute. Ejecuta la instruccion actual anteriormente ensamblada.
   */
  def execute() {
    println("-------------EXECUTE---------")
    instruccionActual match {

      case RET() ⇒ executeRet()
      case CALL(op1) ⇒ executeCall(obtenerValor(op1))
      case JMP(op1) ⇒ executeJMP(obtenerValor(op1))
      case PUSH(op1) ⇒ executePUSH(obtenerValor(op1))
      case POP(op1) ⇒ executePOP(op1)
      case NOT(op1) ⇒ store(op1, ALU.NOT(obtenerValor(op1)))
      case JE(salto) ⇒ executeJMPCondicional(salto, ALU.interpretarBit(cpu.z))
      case JNE(salto) ⇒ executeJMPCondicional(salto, ALU.interpretarBit(ALU.NOT(cpu.z)))
      case JLE(salto) ⇒ executeJMPCondicional(salto, ALU.interpretarBit(ALU.OR(cpu.z, ALU.XOR(cpu.n, cpu.v))))
      case JG(salto) ⇒ executeJMPCondicional(salto, ALU.interpretarBit(ALU.NOT(ALU.OR(cpu.z, ALU.XOR(cpu.n, cpu.v)))))
      case JL(salto) ⇒ executeJMPCondicional(salto, ALU.interpretarBit(ALU.XOR(cpu.n, cpu.v)))
      case JGE(salto) ⇒ executeJMPCondicional(salto, ALU.interpretarBit(ALU.NOT(ALU.XOR(cpu.n, cpu.v))))
      case JLEU(salto) ⇒ executeJMPCondicional(salto, ALU.interpretarBit(ALU.OR(cpu.c, cpu.z)))
      case JGU(salto) ⇒ executeJMPCondicional(salto, ALU.interpretarBit(ALU.NOT(ALU.OR(cpu.c, cpu.z))))
      case JCS(salto) ⇒ executeJMPCondicional(salto, ALU.interpretarBit(cpu.c))
      case JNEG(salto) ⇒ executeJMPCondicional(salto, ALU.interpretarBit(cpu.n))
      case JVS(salto) ⇒ executeJMPCondicional(salto, ALU.interpretarBit(cpu.v))
      case CMP(op1, op2) ⇒ executeCmp(obtenerValor(op1), obtenerValor(op2))
      case MOV(op1, op2) ⇒ store(op1, obtenerValor(op2))
      case AND(op1, op2) ⇒ {
        var mapa = ALU.AND(obtenerValor(op1), obtenerValor(op2))
        cpu.actualizarFlags(mapa)
        store(op1, mapa("resultado").asInstanceOf[W16])
      }
      case OR(op1, op2) ⇒ {
        var mapa = ALU.OR(obtenerValor(op1), obtenerValor(op2))
        cpu.actualizarFlags(mapa)
        store(op1, mapa("resultado").asInstanceOf[W16])
      }
      case iOp2: Instruccion_DosOperandos ⇒ store(iOp2.destino, execute_instruccion_matematica())
    }
    println("Ejecuta la instruccion!!!")
  }

  /**
   * Simula el store. Recibe un valor que es guardado en el modo de direccionamiento enviado por parametro.
   * @param ModoDireccionamento, W16
   */
  def store(modoDir: ModoDireccionamiento, un_valor: W16) {
    var direccion: Int = 0
    modoDir match {
      case Directo(inmediato: Inmediato) ⇒ { direccion = inmediato.getValor().value; busIO.setStateCelda(direccion, STORE); busIO.setValorC(direccion, un_valor); }
      case Indirecto(directo: Directo) ⇒ { direccion = obtenerValor(directo).value; busIO.setStateCelda(direccion, STORE); busIO.setValorC(direccion, un_valor); }
      case RegistroIndirecto(registro: Registro) ⇒ busIO.setValor(obtenerValor(registro).hex, un_valor)
      case r: Registro ⇒
        r.valor = un_valor
        println(s"Se guarda el resutado $un_valor en " + modoDir.toString)
    }
  }
  /**
   * Ejecuta la instruccion RET. Aumenta el stack pointer (sp) y setea en el pc el valor que este tenia al momento del CALL previo.
   *
   */
  def executeRet() {
    cpu.sp.++
    cpu.pc.:=(busIO.memoria.getValor(cpu.sp.toString).toString)
  }
  /**
   * Delega en la ALU la ejecucion del CMP y luego actualiza los flags.
   */
  def executeCmp(op1: W16, op2: W16) {
    var resultados = ALU.execute_cmp(op1, op2)
    cpu.actualizarFlags(resultados)
  }

  /**
   * Ejecuta el JMP, es decir, cambia el valor del pc por el que recibe por parametro que es el que tiene el JMP.
   * @param W16
   */
  def executeJMP(valor: W16) = cpu.pc.:=(valor.hex)

  /**
   *  Ejecuta el JUMP condicional. Recibe el valor de la condicion y si este es verdadero, incrementa el pc segun lo indique el salto.
   *  @param Salto, Boolean
   */
  def executeJMPCondicional(salto: Salto, condicion: Boolean) {
    if (condicion) { cpu.incrementarPc(salto.value) }
  }

  /**
   * Ejecuta el CALL. Guarda el pc segund donde aputa el stack pointer (sp), decrementa el stack pointer y
   * pone en el pc el valor que tiene el CALL para llamar a la subrutina correspondiente.
   * @param W16
   */
  def executeCall(valor: W16) {
    busIO.memoria.setValor(cpu.sp.toString, cpu.pc)
    cpu.sp.--
    cpu.pc.:=(valor.hex)
  }

  /**
   * Ejecuta el PUSH.
   * @param W16
   */
  def executePUSH(valor: W16) {
    busIO.memoria.setValor(cpu.sp.toString, valor)
    cpu.sp.--
  }

  /**
   * Ejecuta el POP aumenta el stack pointer (sp), y guarda en el modo de direccionamiento
   * recibido por parametro el valor que se encuetra en el en esa celda a la que apunta el sp.
   * @param W16
   */
  def executePOP(modoDir: ModoDireccionamiento) {
    cpu.sp.++
    store(modoDir, busIO.memoria.getValor(cpu.sp.toString))
  }
}

object tt extends App {
  var programa = new Programa(List(SUB(R1, R4)))
  //  var sim = Simulador(programa)
  // sim.inicializarSim()
  // sim.cargarPrograma("0003")
}