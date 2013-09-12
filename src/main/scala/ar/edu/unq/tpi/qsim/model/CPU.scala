package ar.edu.unq.tpi.qsim.model

import scala.collection.mutable.ArrayBuffer
import ar.edu.unq.tpi.qsim.utils.Util
import ar.edu.unq.tip.qsim.state.Estado
import ar.edu.unq.tip.qsim.state.Inicial

case class CPU() {
	// VER SI VAMOS A MODELAR EL ESTADO O NO DE LA CPU
  var registros = ArrayBuffer[Registro](R0, R1, R2, R3, R4, R5, R6, R7)
  var pc = new W16("0000")
  var sp = new W16("FFFF")
  var ir = ""
  var alu = ALU

  var n = 0
  var v = 0
  var z = 0
  var c = 0
  
  def registro(registroString:String) = registros.find(_.representacionString == registroString)

  def incrementarPc(cantidad: Int) = {
      println("se incrementa!!")
      pc.++(cantidad)
    }

  def cargarPc(valor: String) {
    //esperar que Tati cambie lo de w16
    println("valor a cargar" + valor)
    pc = new W16(valor)
    println("valor cargado" + pc.toString)
  }

  def actualizarRegistros(registrosAct: Map[String, W16]) {
    for (x <- 0 to 7) {
      if (registrosAct.contains(s"R$x")) {
        this.registros(x).setValor(registrosAct(s"R$x"))
      }
    }
  }

def actualizarFlags(flags: Map[String, Any]) {
    v = flags("v").asInstanceOf[Int]
    z = flags("z").asInstanceOf[Int]
    c = flags("c").asInstanceOf[Int]
    n = flags("n").asInstanceOf[Int]
  }
}

object pur extends App() {

  var cpu = CPU()
  print(cpu.registros + "\n")
  print(cpu.pc)
}