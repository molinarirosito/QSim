package ar.edu.unq.tpi.qsim.ArqQ1

import scala.collection.mutable.ArrayBuffer

import org.scalatest._

import ar.edu.unq.tpi.qsim.model._


class CicloDeEjecucionArquitecturaQ1  extends FlatSpec with Matchers {

  def fixture = 
    new {
	  val simulador = new Simulador()
	  simulador.inicializarSim()
	  var instrucciones = new ArrayBuffer[Instruccion]()
      
      instrucciones.+:(ADD(R0,new Inmediato(new W16("2000"))),MUL(R4,new Inmediato(new W16("0100"))),
    		  		   SUB(R5,new Inmediato(new W16("A000"))),MOV(R5,new Inmediato(new W16("A000"))))
      
      val programa = new Programa(instrucciones)
	  val registrosParaActualizar = Map(("R0", new W16("0986")),("R4", new W16("A200")))
  	}
  
  "Un Simulador" should "inicializar la cpu y la memoria cuando se crea" in {
    val f = fixture
    f.simulador.cpu should be (CPU())
    f.simulador.memoria should be (Memoria(25))
  }
  
  it should "cargar un programa en la memoria desde la posicion que indica pc y actualizar los registros de cpu" in {
	val f  = fixture
	f.simulador.cargarProgramaYRegistros(f.programa,"0000" ,f.registrosParaActualizar)
	
  }

  
}