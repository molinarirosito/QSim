package ar.edu.unq.tpi.qsim.arq

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import ar.edu.unq.tpi.qsim.model._
import scala.collection.mutable._

class ArquirirDatosOperandos extends FlatSpec with Matchers {

  def contexto_operandos = new {
    var inst1 = MUL(R2, R3)
    var inst2 = MOV(R2, new Inmediato(new W16("4000")))
    var inst3 = ADD(R3, new Directo(new Inmediato(new W16("0002"))))
    var inst4 = SUB(new Directo(new Inmediato(new W16("0006"))), R2)
    var inst5 = SUB(new Directo(new Inmediato(new W16("000B"))), new Inmediato(new W16("0010")))
    var inst6 = DIV(new Directo(new Inmediato(new W16("0008"))), new Directo(new Inmediato(new W16("000A"))))
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
    simulador1.cargarProgramaYRegistros(programa1, "000C", Map[String, W16]())

    var programa2 = new Programa(List(contexto.inst2))
    var simulador2 = Simulador()
    simulador2.inicializarSim
    simulador2.cargarProgramaYRegistros(programa2, "000C", Map[String, W16]())

    var programa3 = new Programa(List(contexto.inst3))
    var simulador3 = Simulador()
    simulador3.inicializarSim
    simulador3.busIO.setValor("0002", new W16("0009"))
    simulador3.cargarProgramaYRegistros(programa3, "000C", Map[String, W16]())

    var programa4 = new Programa(List(contexto.inst4))
    var simulador4 = Simulador()
    simulador4.inicializarSim
    simulador4.busIO.setValor("0006", new W16("0001"))
    simulador4.cargarProgramaYRegistros(programa4, "000C", Map[String, W16]())

    var programa5 = new Programa(List(contexto.inst5))
    var simulador5 = Simulador()
    simulador5.inicializarSim
    simulador5.busIO.setValor("000B", new W16("0007"))
    simulador5.cargarProgramaYRegistros(programa5, "000C", Map[String, W16]())

    var programa6 = new Programa(List(contexto.inst6))
    var simulador6 = Simulador()
    simulador6.inicializarSim
    simulador6.busIO.setValor("0008", new W16("0005"))
    simulador6.busIO.setValor("000A", new W16("0004"))
    simulador6.cargarProgramaYRegistros(programa6, "000C", Map[String, W16]())

  }

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

  //  // ------------------------------------------------------------//  
  it should "buscar los datos en los modos de direccionamiento destino ->Registro origen -> Directo y verificar el store" in {
    var ctx = contexto_operandos
    var sim = simuladores
    println(sim.simulador3.busIO.memoria.show("0000"))
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

  //  // --------------------------------------------------------------//
  //  it should "buscar los datos en los modos de direccionamiento destino ->Inmediato origen -> RegistroIndirecto" in {
  //  }
  //  it should "buscar los datos en los modos de direccionamiento destino ->Registro origen -> RegistroIndirecto" in {
  //  }
  //  it should "buscar los datos en los modos de direccionamiento destino ->Directo origen -> RegistroIndirecto" in {
  //  }
  //  it should "buscar los datos en los modos de direccionamiento destino ->RegistroIndirecto origen -> RegistroIndirecto" in {
  //  }
  //  it should "buscar los datos en los modos de direccionamiento destino ->RegistroIndirecto origen -> Directo" in {
  //  }
  //  it should "buscar los datos en los modos de direccionamiento destino ->RegistroIndirecto origen -> Registro" in {
  //  }
  //
  //  // -------------------------------------------------------------//
  //  it should "buscar los datos en los modos de direccionamiento destino ->Inmediato origen -> InDirecto" in {
  //  }
  //  it should "buscar los datos en los modos de direccionamiento destino ->Registro origen -> InDirecto" in {
  //  }
  //  it should "buscar los datos en los modos de direccionamiento destino ->Directo origen -> InDirecto" in {
  //  }
  //  it should "buscar los datos en los modos de direccionamiento destino ->RegistroIndirecto origen -> InDirecto" in {
  //  }
  //  it should "buscar los datos en los modos de direccionamiento destino ->InDirecto origen -> InDirecto" in {
  //  }
  //  it should "buscar los datos en los modos de direccionamiento destino ->InDirecto origen -> RegistroIndirecto" in {
  //  }
  //  it should "buscar los datos en los modos de direccionamiento destino ->InDirecto origen -> Directo" in {
  //  }
}