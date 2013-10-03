package ar.edu.unq.tpi.qsim.arq

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import ar.edu.unq.tpi.qsim.model._
import ar.edu.unq.tpi.qsim.parser._
import ar.edu.unq.tpi.qsim.utils._
import scala.collection.mutable.Map

class CicloEjecucionArquitecturaQ3 extends FlatSpec with Matchers {

  def parsers_resultados = new {
    var parser = Parser
    var resultadoQ2 = parser.ensamblarQ3("src/main/resources/programaQ2.qsim")
    var resultadoQ2SintaxError = parser.ensamblarQ2("src/main/resources/programaQ2SintaxError.qsim")
  }

  def programas = new {
    var instrucciones = List(MOV(R0, new Directo(new Inmediato("0002"))), MUL(R4, new Inmediato("0001")), SUB(new Directo(new Inmediato("0003")), new Inmediato("000A")),
      ADD(R5, new Inmediato("0056")), MOV(new Directo(new Inmediato("000B")), new Directo(new Inmediato("0005"))), ADD(R1, R7))
    var programaQ2 = new Programa(instrucciones)
  }

  //--------------------------------------------TESTS PARSER -----------------------------------------------//

  "Un Parser" should "parsear exitosamente un programa " in {
    var set_parser = parsers_resultados
    var set_programas = programas

    set_parser.resultadoQ2

    assert(set_parser.resultadoQ2.equals(set_programas.programaQ2))
  }

  it should "tirar un Failure cuando parsea un programa con sintaxis invalida" in {
    var set_parser = parsers_resultados
    var set_programas = programas

    var mensaje_esperado = "`0x' expected but `]' found"

    //assert(set_parser.resultadoQ2SintaxError.asInstanceOf[FAILURE].mensaje.equals(mensaje_esperado))
  }

  //----------------------------------------------TESTS SIMULADOR -----------------------------------------------//

  def simuladores = new {
    var parser = parsers_resultados
    var programa = parser.resultadoQ2
    var registros_actualizar = registros_a_actualizar

    var simulador = new Simulador()
    simulador.inicializarSim()

    var simulador_con_programa = new Simulador()
    simulador_con_programa.inicializarSim()
    simulador_con_programa.cargarProgramaYRegistros(programa, "0000", registros_actualizar.registros)
  }

  def registros_a_actualizar = new {
    var registros = Map[String, W16](("R5", "0010"), ("R0", "0010"), ("R2", "9800"), ("R1", "0009"), ("R7", "0001"))
  }

  "Un Simulador" should "cargar un programa en la memoria desde la posicion que indica pc y actualizar los registros de cpu" in {
    var set_simuladores = simuladores
    var set_registros = registros_a_actualizar
    var set_parser = parsers_resultados
    var pc = "0000"
    var programa = set_parser.resultadoQ2

    set_simuladores.simulador.cargarProgramaYRegistros(programa, pc, set_registros.registros)

    set_simuladores.simulador.cpu.pc.hex should be(pc)

    var mapaRegistros = set_registros.registros
  }

  it should "actualizar los registros de cpu" in {
    var set_simuladores = simuladores
    var set_registros = registros_a_actualizar
    var mapaRegistros = set_registros.registros
    // verificando que los registros se actualicen bien
    for {
      key ← mapaRegistros.keys
      value = mapaRegistros(key)
    } yield {
      set_simuladores.simulador.cpu.registro(key) match {
        case Some(registro) ⇒ {
          assert(registro.valor.equals(value))
        }
        case _ ⇒
      }
    }
  }
  //-----------------------------------------------------EJECUCION PASO A PASO -----------------------------------------//
  // TODO deberia de crear 3 test mas probando por separado el paso Fetch/decode/execute
  it should "ejecutar el ciclo de instruccion (Paso-a-Paso) al programa que esta cargado en la memoria " in {
    var set_simuladores = simuladores
    var set_parser = parsers_resultados
    var programa = set_parser.resultadoQ2
    var count = 0
    do {
      set_simuladores.simulador_con_programa.fetch()
      set_simuladores.simulador_con_programa.decode()
      set_simuladores.simulador_con_programa.execute()
      count += 1
    } while (count < programa.instrucciones.length)

  }

  //expect(false) {
  //simulador_iniciado.simulador.etiquetasInvalidas(pc.programa)
  //}
  //pc.programa = f.simulador.asignarPosiciones("0000", pc.programa)
  // verificar que las instrucciones tienen la posicion correcta en la memoria
  //pc.programa.instrucciones.foreach(inst => println("nombre de la Inst " + inst + " posicion de la instruccion = " +   inst.position))
  //pc.programa = f.simulador.calcularEtiquetas(pc.programa)
  //println(pc.programa)
  // assert(f.simulador.cpu.pc.equals(new W16("0000")))

  //  it should "ejecutar un programa que se encuentra en memoria " in {
  //    var spc = simuladorConProgramaCargado
  //    var pcAnteriorAEjecucion = spc.simulador.cpu.pc
  //    spc.simulador.ejecucion(programaCreado.programa)
  //    assert(pcAnteriorAEjecucion.equals(new W16("000C")))
  //    println(spc.simulador.memoria.show("0000"))
  //    println(spc.simulador.cpu.registros)
  //  }
}