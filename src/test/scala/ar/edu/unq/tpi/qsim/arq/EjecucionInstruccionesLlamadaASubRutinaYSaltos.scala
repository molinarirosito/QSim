package ar.edu.unq.tpi.qsim.arq

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import ar.edu.unq.tpi.qsim.model._
import scala.collection.mutable._

class EjecucionInstruccionesLlamadaASubRutinaYSaltos extends FlatSpec with Matchers {
  
   def contexto_operandos = new {
    var instrucciones = List(MOV(R0, new Inmediato(new W16("0002"))), CALL(new Inmediato(new W16("0005"))), MOV( R1, R0), MUL(R0, new Inmediato(new W16("0004"))), RET())
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
  
  "Un CALL" should "guardar el valor del pc actual en la pila y actualizar el valor de pc con el operado origen" in {
  
    
  }
  
  "Un RET" should "buscar el valor del tope de la pila y actualizar el pc con ese valor" in {
    
  }

}