package ar.edu.unq.tpi.qsim.arq

import scala.collection.mutable.Map
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.uqbar.commons.utils.Observable
import ar.edu.unq.tpi.qsim.model.ADD
import ar.edu.unq.tpi.qsim.model.CALL
import ar.edu.unq.tpi.qsim.model.Directo
import ar.edu.unq.tpi.qsim.model.Etiqueta
import ar.edu.unq.tpi.qsim.model.Inmediato
import ar.edu.unq.tpi.qsim.model.MOV
import ar.edu.unq.tpi.qsim.model.MUL
import ar.edu.unq.tpi.qsim.model.Programa
import ar.edu.unq.tpi.qsim.model.R0
import ar.edu.unq.tpi.qsim.model.R1
import ar.edu.unq.tpi.qsim.model.R4
import ar.edu.unq.tpi.qsim.model.R5
import ar.edu.unq.tpi.qsim.model.RET
import ar.edu.unq.tpi.qsim.model.SUB
import ar.edu.unq.tpi.qsim.model.Simulador
import ar.edu.unq.tpi.qsim.model.W16
import ar.edu.unq.tpi.qsim.parser.Parser
import ar.edu.unq.tpi.qsim.utils.stringToW16
import ar.edu.unq.tpi.qsim.exeptions.SyntaxErrorException

class CicloEjecucionArquitecturaQ3 extends FlatSpec with Matchers {

  def parsers_resultados = new {
    var parser = Parser
    var resultadoQ3 = parser.ensamblarQ3("src/main/resources/programaQ3.qsim")
  }

  def programas = new {
    var instrucciones = List(ADD(R0, new Directo(new Inmediato("0002"))), MUL(R4, new Inmediato("0001")), SUB(new Directo(new Inmediato("0003")), new Inmediato("000A")),
      MOV(R5, new Inmediato("0056")), MOV(new Directo(new Inmediato("0005")), new Etiqueta("etiqueta")), CALL(new Directo(new Inmediato("0005"))), MOV(R0, R1), ADD(R5, R0),
      ADD(R0, new Inmediato("0002")), RET())
    var programaQ3 = new Programa(instrucciones)
    programaQ3.etiquetas("etiqueta") = ADD(R0, new Inmediato("0002"))
    var instruccionesinterpretadas = List("2808 0002", "0900 0001", "3200 0003 000A", "1940 0056", "1200 0005 0010", "B008 0005", "1821 ", "2960 ", "2800 0002", "C000")
    var instruccionesdecodificadas = List("ADD R0, [0x0002]", "MUL R4, 0x0001", "SUB [0x0003], 0x000A", "MOV R5, 0x0056", "MOV [0x0005], 0x0010", "CALL [0x0005]", "MOV R0, R1", "ADD R5, R0", "ADD R0, 0x0002", "RET")
  }

  //--------------------------------------------TESTS PARSER -----------------------------------------------//

  "Un Parser" should "parsear exitosamente un programa " in {
    var set_parser = parsers_resultados
    var set_programas = programas

    set_parser.resultadoQ3

    assert(set_parser.resultadoQ3.equals(set_programas.programaQ3))
  }

  it should "tirar un Failure cuando parsea un programa con sintaxis invalida" in {
    var set_parser = parsers_resultados
    var set_programas = programas

    var mensaje_esperado = "Ha ocurrido un error en la linea 6 CALL [0x0005]"

    val exception = intercept[SyntaxErrorException] {
      set_parser.parser.ensamblarQ2("src/main/resources/programaQ3SyntaxError.qsim")
    }
    assert(exception.getMessage().equals(mensaje_esperado))
  }

  //  //----------------------------------------------TESTS SIMULADOR -----------------------------------------------//
  //
  def simuladores = new {
    var parser = parsers_resultados
    var programa = parser.resultadoQ3
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
    var programa = set_parser.resultadoQ3

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
    var programa = set_parser.resultadoQ3
    var instrucciones = programas
    var count = 0
    do {
      set_simuladores.simulador_con_programa.fetch()
      //FETCH
      assert(instrucciones.instruccionesinterpretadas(count) === set_simuladores.simulador_con_programa.cpu.ir)
      //DECODE
      var decode = set_simuladores.simulador_con_programa.decode()
      assert(instrucciones.instruccionesdecodificadas(count) === decode)

      count += 1
    } while (count < programa.instrucciones.length)

  }

}