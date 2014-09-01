package ar.edu.unq.tpi.qsim.model

import java.util.Calendar
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map
import org.uqbar.commons.utils.Observable
import ar.edu.unq.tpi.qsim.utils.Util
import ar.edu.unq.tpi.qsim.exeptions._
import ar.edu.unq.tpi.qsim.parser._


@Observable
object Ciclo {
  var fetch = true
  var decode = false
  var execute = false
  var execute_complete = true
  
  def ninguna_etapa() {
    fetch = false
    decode = false
    execute = false
    execute_complete = false
  }

  def pasarAFetch() {
    fetch = true
    execute = false
    execute_complete = true
  }
  def pasarADecode() {
    fetch = false
    decode = true
    execute_complete = false
  }
  def pasarAExecute() {
    decode = false
    execute = true
    execute_complete = false
  }
}

@Observable
case class Simulador() {

  var ciclo = Ciclo
  var mensaje_al_usuario = ""
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
    busIO.initialize()
    agregarMensaje("******************INFORMACION*******************")
    agregarMensaje("El programa compilado ha sido cargado en la memoria con exito")
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
      case inst_sc: JUMP_condicional ⇒ verificarOperandoEtiqueta(inst_sc.desplazamiento.asInstanceOf[SaltoEtiqueta].etiqueta, programa)
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
  def calcularValorOrigenEtiqueta(instruccion: Instruccion_DosOperandos, programa: Programa) = {
    var origen = instruccion.origen
    if (instruccion.origen.codigo.equals("111111")) {
      origen = new Inmediato(programa.etiquetas(instruccion.origen.representacionString).position)
    }
    origen
  }
  /**
   * Calcula de acuerdo al operando que le pasan el valor de la etiqueta
   * @param Operando , Programa
   * @return W16
   */
  def calcularValorOperandoEtiqueta(instruccion: Instruccion_UnOperando, programa: Programa) = {
    var operando = instruccion.operando
    if (instruccion.operando.codigo.equals("111111")) {
      operando = new Inmediato(programa.etiquetas(instruccion.operando.representacionString).position)
    }
    operando
  }

  /**
   * Calcula de acuerdo al operando que le pasan el valor de la etiqueta
   * @param Operando , Programa
   * @return W16
   */
  def calcularValorSaltoEtiqueta(instruccion: JUMP_condicional, programa: Programa) = {
    var resultado = instruccion.desplazamiento.salto
    if (instruccion.desplazamiento.asInstanceOf[SaltoEtiqueta].etiqueta.codigo.equals("111111")) {
      instruccion.position.++
      var posicionActual = instruccion.position
      var posicionASaltar = programa.etiquetas(instruccion.desplazamiento.asInstanceOf[SaltoEtiqueta].etiqueta.representacionString).position
      resultado = posicionASaltar.value - posicionActual.value
    }
    if (resultado >= -128 & resultado <= 127) {
      Util.binaryToInteger(Util.intToCa2_8B(resultado))
    } else {
      throw new DesplazamientoSaltoInvalidoException("Revisar los saltos utilizados, uno desplazamiento sobrepasa el limite permitido.")
    }
  }

  def calcularEtiquetas(programa: Programa): Programa = {
    programa.instrucciones.foreach(inst ⇒ {
      inst match {
        case inst_dp: Instruccion_DosOperandos ⇒ inst_dp.origen = calcularValorOrigenEtiqueta(inst_dp, programa)
        case inst_up: Instruccion_UnOperando ⇒ inst_up.operando = calcularValorOperandoEtiqueta(inst_up, programa)
        case inst_sc: JUMP_condicional ⇒ inst_sc.desplazamiento.salto = calcularValorSaltoEtiqueta(inst_sc, programa)
        case inst ⇒
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
      ciclo.ninguna_etapa
      ciclo.pasarAFetch
    } else {
      throw new EtiquetaInvalidaException("Una de las etiquetas utilizadas es invalida")
      println("ERROR ------- ETIQUETAS INVALIDAS -----NO SE CARGA EN MEMORIA!! ")
    }

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
    cambiarEstadoCeldasInstruccionActual(State.EXECUTED)
    val cadena_binaria = obtenerProximaInstruccionBinario()
    instruccionActual = Interprete.interpretarInstruccion(cadena_binaria)
    val instruccion_fech = instruccionActual.representacionHexadecimal()
    println("------Trajo la instruccion a Ejecutar que apunta pc :" + instruccion_fech)
    cpu.ir = instruccion_fech
    agregarMensaje("La intruccion actual ocupa: " + instruccionActual.cantidadCeldas().toString)
    celdaInstruccionActual = obtenerCeldasInstruccionActual()
    cambiarEstadoCeldasInstruccionActual(State.FECH_DECODE)
    cpu.incrementarPc(instruccionActual.cantidadCeldas())
    ciclo.pasarADecode
    println("Cual es el valor de Pc luego del Fetch: " + cpu.pc)

  }

  def obtenerCeldasInstruccionActual(): ArrayBuffer[Celda] =
    {
      busIO.memoria.getCeldas(cpu.pc.value, instruccionActual.cantidadCeldas())

    }

  def cambiarEstadoCeldasInstruccionActual(estado: State.Type) {

    celdaInstruccionActual.foreach(celda ⇒
      celda.state = estado)
  }

  /**
   * Simula el decode de la instruccion. Simplemente muestra lo que ensamblo.
   *
   */
  def decode() = {
    println("----------DECODE------------")
    agregarMensaje("Se decodifico la instruccion : " + (instruccionActual.toString))
    println(mensaje_al_usuario)
    ciclo.pasarAExecute
    (instruccionActual.toString)
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
   * Simula el fetch decode and execute. Ejecuta todas las etapas de ciclo de instruccion a la ves,
   * de la instruccion actual anteriormente ensamblada.
   */
  def execute_complete() {
	  fetch()
	  decode()
	  execute()
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
    ciclo.pasarAFetch
    println("Ejecuta la instruccion!!!")
  }

  /**
   * Simula el store. Recibe un valor que es guardado en el modo de direccionamiento enviado por parametro.
   * @param ModoDireccionamento, W16
   */
  def store(modoDir: ModoDireccionamiento, un_valor: W16) {
    var direccion: Int = 0
    modoDir match {
      case Inmediato(valor: W16) ⇒ { ciclo.pasarAFetch(); throw new ModoDeDireccionamientoInvalidoException("Un Inmediato no puede ser un operando destino."); }
      case Directo(inmediato: Inmediato) ⇒ { direccion = inmediato.getValor().value; busIO.setValorC(direccion, un_valor); busIO.setStateCelda(direccion, State.STORE); }
      case Indirecto(directo: Directo) ⇒ { direccion = obtenerValor(directo).value; busIO.setValorC(direccion, un_valor); busIO.setStateCelda(direccion, State.STORE); }
      case RegistroIndirecto(registro: Registro) ⇒ { direccion = obtenerValor(registro).value; busIO.setValorC(direccion, un_valor); busIO.setStateCelda(direccion, State.STORE); }
      case r: Registro ⇒
        r.valor = un_valor
        println(s"Se guarda el resutado $un_valor en " + modoDir.toString)
        agregarMensaje(s"Se guardado el resutado $un_valor en " + modoDir.toString)
    }
  }
  /**
   * Ejecuta la instruccion RET. Aumenta el stack pointer (sp) y setea en el pc el valor que este tenia al momento del CALL previo.
   *
   */
  def executeRet() {

    if (cpu.sp.value >= busIO.memoria.tamanioMemoria - 1) {
      throw new StackPointerExeption("Error, Estado de Pila Vacia.")
    }
    cpu.sp.++
    cpu.pc.:=(busIO.getValor(cpu.sp).toString)
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
    if (condicion) {
      var desplazamiento = sacarSaltoCA2(salto.salto)
      cpu.incrementarPc(desplazamiento)
    }
  }

  def sacarSaltoCA2(salto: Int) = {
    var saltoCa2 = salto
    if (salto > 127) {
      saltoCa2 = (-1) * Util.binaryToInteger(Util.representarNumeroEnCA2(salto))
    }
    saltoCa2
  }
  /**
   * Ejecuta el CALL. Guarda el pc segund donde aputa el stack pointer (sp), decrementa el stack pointer y
   * pone en el pc el valor que tiene el CALL para llamar a la subrutina correspondiente.
   * @param W16
   */
  def executeCall(valor: W16) {
    val prepararValorsp = new W16(cpu.sp.toString)
    val prepararValorpc = new W16(cpu.pc.toString)
    store(Directo(Inmediato(prepararValorsp)), prepararValorpc)
    if (cpu.sp.value == 65520) {
      val nuevo_sp = (busIO.memoria.tamanioMemoria - 1)
      cpu.sp.:=(Util.toHex4(nuevo_sp))
    } else { cpu.sp.-- }
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

  def obtenerHora(): String = {
    val hoy = Calendar.getInstance().getTime()

    "[" + hoy.getDate().toString() + "/" + hoy.getMonth().toString + " " + hoy.getHours().toString + ":" + hoy.getMinutes().toString + "]"
  }
  def agregarMensaje(mensaje: String) {
    mensaje_al_usuario = mensaje_al_usuario + obtenerHora + " " + mensaje + "\n"

  }
}

object tt extends App {
  //var l = ArrayBuffer[Int]()
  //l.+=(1)
  //l.+=(2)
  //println(l)
  
//  var programa = Parser.parse("""
//MOV R5, 0x0001
//MOV R2, 0xFFE0 
//ADD R2, R5""", Parser.programQ5).get
//  var sim = Simulador()
//  sim.inicializarSim()
//  sim.busIO.memoria.cargarPrograma(programa,"0000")
}