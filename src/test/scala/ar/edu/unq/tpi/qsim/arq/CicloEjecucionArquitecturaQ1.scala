package ar.edu.unq.tpi.qsim.arq

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map
import org.scalatest._
import ar.edu.unq.tpi.qsim.parser._
import ar.edu.unq.tpi.qsim.model._
import ar.edu.unq.tpi.qsim.utils._
import ar.edu.unq.tpi.qsim.exeptions.SyntaxErrorException

class CicloDeEjecucionArquitecturaQ1 extends FlatSpec with Matchers {

  def parsers_resultados = new {
    var parser = Parser
    var resultadoQ1 = parser.ensamblarQ1("src/main/resources/programaQ1.qsim")
  }

  def programas = new {
    var instrucciones = List(ADD(R0, new Inmediato("0002")), MUL(R4, new Inmediato("0001")), SUB(R5, new Inmediato("000A")),
      MOV(R5, new Inmediato("0056")), MOV(R2, R3), ADD(R1, R7))
    var programaQ1 = new Programa(instrucciones)
    var instruccionesinterpretadas = List("2800 0002", "0900 0001", "3940 000A", "1940 0056", "18A3 ", "2867 ")
    var instruccionesdecodificadas = List("ADD R0 0002", "MUL R4 0001", "SUB R5 000A", "MOV R5 0056", "MOV R2 R3", "ADD R1 R7")

  }
  //--------------------------------------------TESTS PARSER -----------------------------------------------//

  "Un Parser" should "parsear exitosamente un programa " in {
    var set_parser = parsers_resultados
    var set_programas = programas

    assert(set_parser.resultadoQ1.equals(set_programas.programaQ1))
  }

  it should "tirar un Failure cuando parsea un programa con sintaxis invalida" in {
    var set_parser = parsers_resultados
    var set_programas = programas
    var mensaje_esperado = "A ocurrido un error en la linea 2 MUL R4, 0x01"

    val exception = intercept[SyntaxErrorException] {
      set_parser.parser.ensamblarQ1("src/main/resources/programaQ1SyntaxError.qsim")
    }
    assert(exception.getMessage().equals(mensaje_esperado))
  }

  //----------------------------------------------TESTS SIMULADOR -----------------------------------------------//

  def simuladores = new {
    var parser = parsers_resultados
    var programa = parser.resultadoQ1
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

  "Un Simulador" should "cargar un programa en la memoria desde la posicion que indica pc" in {
    var set_simuladores = simuladores
    var set_registros = registros_a_actualizar
    var set_parser = parsers_resultados
    var pc = "0000"
    var programa = set_parser.resultadoQ1

    set_simuladores.simulador.cargarProgramaYRegistros(programa, pc, set_registros.registros)

    // verificar que pc tiene el valor esperado
    set_simuladores.simulador.cpu.pc.hex should be(pc)
    // TODO TESTEAR QUE DENTRO DE LA MEMORIA EXISTA TAL PROGRAMA!!!
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
  it should "ejecutar el ciclo de instruccion FETCH - DECODE (Paso-a-Paso) al programa que esta cargado en la memoria " in {
    var set_simuladores = simuladores
    var set_parser = parsers_resultados
    var programa = set_parser.resultadoQ1
    var instrucciones = programas
    var count = 0
    do {
      //FETCH
      set_simuladores.simulador_con_programa.fetch()
      assert(instrucciones.instruccionesinterpretadas(count) === set_simuladores.simulador_con_programa.cpu.ir)
      //DECODE
      var decode = set_simuladores.simulador_con_programa.decode()
      assert(instrucciones.instruccionesdecodificadas(count) === decode)
      
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
