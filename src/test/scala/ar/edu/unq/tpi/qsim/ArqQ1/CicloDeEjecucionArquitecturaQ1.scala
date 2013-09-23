package ar.edu.unq.tpi.qsim.ArqQ1

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map

import org.scalatest._
import ar.edu.unq.tpi.qsim.parser._
import ar.edu.unq.tpi.qsim.model._
import ar.edu.unq.tpi.qsim.utils._

class CicloDeEjecucionArquitecturaQ1 extends FlatSpec with Matchers {

  def parsers_resultados = new {
    var parser = Parser
    var resultadoQ1 = parser.ensamblar("src/main/resources/programaQ1.qsim")
    var resultadoQ1SintaxError = parser.ensamblar("src/main/resources/programaQ1SintaxError.qsim")
  }

  def programas = new {
    var instrucciones = List(ADD(R0, new Inmediato("0002")), MUL(R4, new Inmediato("0001")), SUB(R5, new Inmediato("000A")),
      MOV(R5, new Inmediato("0056")), MOV(R2, R3), ADD(R1, R7))
    var programaQ1 = new Programa(instrucciones)
  }

  //--------------------------------------------TESTS PARSER -----------------------------------------------//

  "Un Parser" should "parsear exitosamente un programa " in {
    var set_parser = parsers_resultados
    var set_programas = programas

    // verificando si el resultado dio Ok!!!
    set_parser.resultadoQ1.estado should be("OK")

    // verificando que el programa parseado sea el esperado
    assert(set_parser.resultadoQ1.asInstanceOf[OK].resultado.equals(set_programas.programaQ1))
  }

  it should "tirar un Failure cuando quiere parsear un programa donde la sintaxis no es correcta" in {
    var set_parser = parsers_resultados
    var set_programas = programas

    // verificando si el resultado dio Failure!!!
    set_parser.resultadoQ1SintaxError.estado should be("FAILURE")
    // verificando el mensaje del failure

    //assert(set_parser.resultadoQ1.asInstanceOf[FAILURE].mensaje.equals(set_programas.programaQ1))

  }

  //----------------------------------------------TESTS SIMULADOR -----------------------------------------------//

  def simuladorInicializado = new {
    var simulador = new Simulador()
    simulador.inicializarSim()
  }

  //def simuladorConProgramaCargado = new {
   // var programacreado = programa
   // var simuladorinit = simuladorInicializado
   // simuladorinit.simulador.inicializarSim()
    // simuladorinit.simulador.cargarProgramaYRegistros(programacreado.programa, "0000", programacreado.registrosParaActualizar)
    // var simulador = simuladorinit.simulador
 // }

//  "Un Simulador" should "cargar un programa en la memoria desde la posicion que indica pc y actualizar los registros de cpu" in {
//    var simulador_iniciado = simuladorInicializado
//    var set_programas = programas 
//  
//  //  simulador_iniciado.simulador.cargarProgramaYRegistros(set_programas.programaQ1, "0000", simulado)
    
//    expect(false) {
//      simulador_iniciado.simulador.etiquetasInvalidas(pc.programa)
//    }
//    pc.programa = f.simulador.asignarPosiciones("0000", pc.programa)
//    // verificar que las instrucciones tienen la posicion correcta en la memoria
//    //pc.programa.instrucciones.foreach(inst => println("nombre de la Inst " + inst + " posicion de la instruccion = " +   inst.position))
//    //pc.programa = f.simulador.calcularEtiquetas(pc.programa)
//    //println(pc.programa)
//    assert(f.simulador.cpu.pc.equals(new W16("0000")))
//
//    println("-------------Verificar registros actualizados-----------------")
//
//    var map = pc.registrosParaActualizar
//
//    for {
//      key ← map.keys
//      value = map(key)
//    } yield {
//      f.simulador.cpu.registro(key) match {
//        case Some(registro) ⇒ {
//          println("valor del registro " + registro.valor.toString)
//          println("valor a actualizar " + value.toString)
//          assert(registro.valor.equals(value))
//        }
//        case _ ⇒
//      }
//    }
//
//  }

//  it should "ejecutar un programa que se encuentra en memoria " in {
//    var spc = simuladorConProgramaCargado
//    var pcAnteriorAEjecucion = spc.simulador.cpu.pc
//    spc.simulador.ejecucion(programaCreado.programa)
//    assert(pcAnteriorAEjecucion.equals(new W16("000C")))
//    println(spc.simulador.memoria.show("0000"))
//    println(spc.simulador.cpu.registros)
//  }
}
