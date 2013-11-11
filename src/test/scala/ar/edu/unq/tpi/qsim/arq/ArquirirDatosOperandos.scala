package ar.edu.unq.tpi.qsim.arq

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import ar.edu.unq.tpi.qsim.model._
import scala.collection.mutable._
import ar.edu.unq.tpi.qsim.utils._

class ArquirirDatosOperandos extends FlatSpec with Matchers {

  def contexto_operandos = new {
    //////Q1
    var inst1 = MUL(R2, R3)
    var inst2 = MOV(R2, new Inmediato("4000"))
    //////Q2
    var inst3 = ADD(R3, new Directo(new Inmediato("0002")))
    var inst4 = SUB(new Directo(new Inmediato("0006")), R2)
    var inst5 = SUB(new Directo(new Inmediato("000B")), new Inmediato("0010"))
    var inst6 = DIV(new Directo(new Inmediato("0008")), new Directo(new Inmediato("000A")))
    //////Q5
    var inst7 = CMP(new Indirecto(new Directo(new Inmediato("000E"))), new RegistroIndirecto(R4))
    var inst8 = ADD(new RegistroIndirecto(R0), new Indirecto(new Directo(new Inmediato("000A"))))
  }

  def simuladores = new {

    var contexto = contexto_operandos
    var programa1 = new Programa(List(contexto.inst1))
    var simulador1 = Simulador()
    simulador1.inicializarSim
    var registro3 = simulador1.cpu.registros(3)
    registro3.valor = new W16("1234")
    var registro2 = simulador1.cpu.registros(2)
    registro2.valor = new W16("0004")
    simulador1.cargarProgramaYRegistros(programa1, "0010", Map[String, W16]())

    var programa2 = new Programa(List(contexto.inst2))
    var simulador2 = Simulador()
    simulador2.inicializarSim
    simulador2.cargarProgramaYRegistros(programa2, "0010", Map[String, W16]())

    var programa3 = new Programa(List(contexto.inst3))
    var simulador3 = Simulador()
    simulador3.inicializarSim
    simulador3.busIO.setValor("0002", new W16("0009"))
    simulador3.cargarProgramaYRegistros(programa3, "0010", Map[String, W16]())

    var programa4 = new Programa(List(contexto.inst4))
    var simulador4 = Simulador()
    simulador4.inicializarSim
    simulador4.busIO.setValor("0006", new W16("0001"))
    simulador4.cargarProgramaYRegistros(programa4, "0010", Map[String, W16]())

    var programa5 = new Programa(List(contexto.inst5))
    var simulador5 = Simulador()
    simulador5.inicializarSim
    simulador5.busIO.setValor("000B", new W16("0007"))
    simulador5.cargarProgramaYRegistros(programa5, "0010", Map[String, W16]())

    var programa6 = new Programa(List(contexto.inst6))
    var simulador6 = Simulador()
    simulador6.inicializarSim
    simulador6.busIO.setValor("0008", new W16("0005"))
    simulador6.busIO.setValor("000A", new W16("0004"))
    simulador6.cargarProgramaYRegistros(programa6, "0010", Map[String, W16]())

    var programa7 = new Programa(List(contexto.inst7))
    var simulador7 = Simulador()
    simulador7.inicializarSim
    simulador7.cpu.registros(4).valor = new W16("0004")
    simulador7.busIO.setValor("0004", new W16("000E"))
    simulador7.busIO.setValor("000E", new W16("1000"))
    simulador7.busIO.setValor("1000", new W16("0001"))
    simulador7.cargarProgramaYRegistros(programa7, "0010", Map[String, W16]())

    var programa8 = new Programa(List(contexto.inst8))
    var simulador8 = Simulador()
    simulador8.inicializarSim
    simulador8.cpu.registros(0).valor = new W16("0004")
    simulador8.busIO.setValor("0004", new W16("0004"))
    simulador8.busIO.setValor("000A", new W16("0005"))
    simulador8.busIO.setValor("0005", new W16("0004"))
    simulador8.cargarProgramaYRegistros(programa8, "0010", Map[String, W16]())

  }
  //------------------------------------------------------Q1

  "Una Instruccion" should "buscar los datos en los modos de direccionamiento destino ->Registro origen -> Registro y verificar el store" in {
    var ctx = contexto_operandos
    var sim = simuladores
    sim.simulador1.fetch()

    var instruccionActual = sim.simulador1.instruccionActual.asInstanceOf[Instruccion_DosOperandos]
    var destino = instruccionActual.destino.getValor()
    var origen = instruccionActual.origen.getValor()
    assert(sim.registro2.getValor.equals(destino))
    assert(sim.registro3.getValor.equals(origen))

    sim.simulador1.decode()
    sim.simulador1.execute()

    assert(sim.simulador1.cpu.registros(2).getValor().equals(new W16("48D0")))
  }

  it should "buscar los datos en los modos de direccionamiento destino ->Registro origen -> Inmediato y verificar el store" in {
    var ctx = contexto_operandos
    var sim = simuladores
    sim.simulador2.fetch()

    var instruccionActual = sim.simulador2.instruccionActual.asInstanceOf[Instruccion_DosOperandos]
    var destino = instruccionActual.destino.getValor()
    var origen = instruccionActual.origen.getValor()

    assert(sim.registro2.getValor.equals(destino))
    assert(ctx.inst2.origen.getValor.equals(origen))

    sim.simulador2.decode()
    sim.simulador2.execute()

    assert(sim.simulador2.cpu.registros(2).getValor().equals(new W16("4000")))
  }

  // ---------------------------------------------------Q2---------
  it should "buscar los datos en los modos de direccionamiento destino ->Registro origen -> Directo y verificar el store" in {
    var ctx = contexto_operandos
    var sim = simuladores
    sim.simulador3.fetch()

    var instruccionActual = sim.simulador3.instruccionActual.asInstanceOf[Instruccion_DosOperandos]
    var destino = instruccionActual.destino.getValor()
    var origen = sim.simulador3.obtenerValor(instruccionActual.origen)
    println(origen)
    assert(sim.registro3.getValor.equals(destino))
    assert(sim.simulador3.obtenerValor(ctx.inst3.origen) === origen)

    sim.simulador3.decode()
    sim.simulador3.execute()

    assert(sim.simulador3.cpu.registros(3).getValor().equals(new W16("123D")))
  }

  it should "buscar los datos en los modos de direccionamiento destino ->Directo origen -> Registro y verificar el store" in {
    var ctx = contexto_operandos
    var sim = simuladores
    sim.simulador4.fetch()

    var instruccionActual = sim.simulador4.instruccionActual.asInstanceOf[Instruccion_DosOperandos]
    var destino = sim.simulador4.obtenerValor(instruccionActual.destino)
    var origen = instruccionActual.origen.getValor()

    assert(sim.simulador4.obtenerValor(ctx.inst4.destino).equals(destino))
    assert(sim.registro2.getValor.equals(origen))
    sim.simulador4.decode()
    sim.simulador4.execute()

    assert(sim.simulador4.obtenerValor(ctx.inst4.destino).equals(new W16("FFFD")))
  }

  it should "buscar los datos en los modos de direccionamiento destino ->Directo origen -> Directo y verificar el store" in {
    var ctx = contexto_operandos
    var sim = simuladores
    sim.simulador5.fetch()

    var instruccionActual = sim.simulador5.instruccionActual.asInstanceOf[Instruccion_DosOperandos]
    var destino = sim.simulador5.obtenerValor(instruccionActual.destino)
    var origen = sim.simulador5.obtenerValor(instruccionActual.origen)

    assert(sim.simulador5.obtenerValor(ctx.inst5.destino).equals(destino))
    assert(sim.simulador5.obtenerValor(ctx.inst5.origen).equals(origen))
    sim.simulador5.decode()
    sim.simulador5.execute()

    assert(sim.simulador5.obtenerValor(ctx.inst5.destino).equals(new W16("FFF7")))
  }

  it should "buscar los datos en los modos de direccionamiento destino ->Directo origen -> Inmediato y verificar el store" in {
    var ctx = contexto_operandos
    var sim = simuladores
    sim.simulador6.fetch()

    var instruccionActual = sim.simulador6.instruccionActual.asInstanceOf[Instruccion_DosOperandos]
    var destino = sim.simulador6.obtenerValor(instruccionActual.destino)
    var origen = instruccionActual.origen.getValor()
    println(destino)
    assert(sim.simulador6.obtenerValor(ctx.inst6.destino).equals(destino))
    assert(ctx.inst6.origen.getValor.equals(origen))
    sim.simulador6.decode()
    sim.simulador6.execute()

    assert(sim.simulador6.obtenerValor(ctx.inst6.destino).equals(new W16("0001")))
  }

  // ------------------------------------Q5--------------------------//

  it should "buscar los datos en los modos de direccionamiento destino ->InDirecto origen -> RegistroIndirecto" in {
    var ctx = contexto_operandos
    var sim = simuladores
    sim.simulador7.fetch()

    var instruccionActual = sim.simulador7.instruccionActual.asInstanceOf[Instruccion_DosOperandos]
    var destino = sim.simulador7.obtenerValor(instruccionActual.destino)
    var origen = instruccionActual.origen.getValor()
    assert(sim.simulador7.obtenerValor(ctx.inst7.destino).equals(destino))
    assert(ctx.inst7.origen.getValor.equals(origen))
    sim.simulador7.decode()
    sim.simulador7.execute()

    assert(sim.simulador7.obtenerValor(ctx.inst7.destino).equals(new W16("0001")))
  }

  it should "buscar los datos en los modos de direccionamiento destino ->RegistroIndirecto origen -> InDirecto" in {
    var ctx = contexto_operandos
    var sim = simuladores
    sim.simulador8.fetch()

    var instruccionActual = sim.simulador8.instruccionActual.asInstanceOf[Instruccion_DosOperandos]
    var destino = sim.simulador8.obtenerValor(instruccionActual.destino)
    println(destino)
    var origen = instruccionActual.origen.getValor()
    println(origen)
    assert(sim.simulador8.obtenerValor(ctx.inst8.destino).equals(destino))
    assert(ctx.inst8.origen.getValor.equals(origen))
    sim.simulador8.decode()
    sim.simulador8.execute()

     assert(sim.simulador8.obtenerValor(ctx.inst8.destino).equals(new W16("0008")))
  }
}