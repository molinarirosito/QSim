package ar.edu.unq.tip.qsim.integracion.mumuki

import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import org.uqbar.commons.utils.Observable
import ar.edu.unq.tpi.qsim.model.Programa
import ar.edu.unq.tpi.qsim.parser.ArquitecturaQ
import ar.edu.unq.tpi.qsim.parser.Parser

class QsimMainMumuki {
  
  var files: java.util.List[Archivo] = scala.collection.immutable.List[Archivo]()
  var current: Archivo = _
  var arqCurrent: ArquitecturaQ = _
  var program: Programa = _
  var programCounter = "0000"

  def setPathFile(path: String) {
    if (path != null) {
      var nombre = takeName(path)
      var codigo = readFile(path)
      println(codigo)
      var archivo = new Archivo(nombre, codigo)
      files = files.+:(archivo)
      println(files)
    }
  }

  def getPathfile() = ""

  def readFile(path: String) = {
    val input = io.Source.fromFile(path)
    input.mkString
  }

  def ensamblar() {
    program = null
    println("QUE ARQUITECTURA USAMOS \n" + arqCurrent)
    program = arqCurrent.parser(files.map(_.codigo).mkString)
    println("RESULTADO DEL ENSAMBLADO \n" + program)
  }
  
  def selectArqQ(arqQ: Integer) {
    arqCurrent = Parser.arquitecturas(arqQ)
  }
  
  def takeName(path: String) = {
    var part_path = path.split("/")
    part_path(part_path.length - 1)
  }
  
}