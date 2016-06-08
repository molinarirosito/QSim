package ar.edu.unq.tip.qsim.integracion.mumuki

import ar.edu.unq.tpi.qsim.model.Simulador
import ar.edu.unq.tpi.qsim.model.W16
import scala.collection.mutable.Map
import ar.edu.unq.tpi.qsim.exeptions.SyntaxErrorException
import ar.edu.unq.tpi.qsim.parser.Parser

object runMainMumuki extends App {

  var program = args(0)
  var arqQ = args(1).toInt
  var la = new QsimMainMumuki()
  var sim = Simulador()
  try {
    la.setPathFile("src/main/resources/" + program)
    la.selectArqQ(arqQ)
    la.ensamblar()
    sim.inicializarSim()
    sim.cargarProgramaYRegistros(la.program, "0000", Map[String, W16]())
    sim.execute_all_program();
  } catch {
    case ex: SyntaxErrorException => {
    }
  }
} 
