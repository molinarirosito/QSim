package ar.edu.unq.tpi.qsim.arq

import scala.collection.mutable.Map

case class PathTest {

  var paths: Map[String, (String, String)] = Map[String, (String, String)](
    ("Q1" -> ("src/main/resources/casos-test/programaQ1.qsim", "src/main/resources/casos-test/programaQ1SyntaxError.qsim")),
    ("Q2" -> ("src/main/resources/casos-test/programaQ2.qsim", "src/main/resources/casos-test/programaQ2SyntaxError.qsim")),
    ("Q3" -> ("src/main/resources/casos-test/programaQ3.qsim", "src/main/resources/casos-test/programaQ3SyntaxError.qsim")),
    ("Q4" -> ("src/main/resources/casos-test/programaQ4.qsim", "src/main/resources/casos-test/programaQ4SyntaxError.qsim")),
    ("Q5" -> ("src/main/resources/casos-test/programaQ5.qsim", "src/main/resources/casos-test/programaQ5SyntaxError.qsim")),
    ("Q6" -> ("src/main/resources/casos-test/programaQ6.qsim", "src/main/resources/casos-test/programaQ6SyntaxError.qsim")),
    ("CALLRET" -> ("src/main/resources/casos-test/programaQ3CALLRET.qsim", "src/main/resources/casos-test/programaSaltos.qsim")),
    ("JMP" -> ("src/main/resources/casos-test/programCon.qsim", "src/main/resources/casos-test/programaSaltos.qsim")))

  def getContenido(arqQ: String, elegido: Int): String = {
    var pathQ = ""
    if (elegido == 1) {
      pathQ = paths(arqQ)._1
    } else {
      pathQ = paths(arqQ)._2
    }
    readFile(pathQ)
  }

  def readFile(path: String): String = {
    val input = io.Source.fromFile(path)
    return input.mkString
  }
}