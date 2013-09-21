package ar.edu.unq.tpi.qsim.parser

class Parser(var path: String) extends ParserEnsamblador {

  def parsear() = {
    val input = io.Source.fromFile(path)
    val str = input.mkString

    parse(str) match {
      case Success(result, _) ⇒ (result)
      case Failure(msg, i) ⇒ println("[Failure] " + s" $msg in $i")
      case Error(msg, i) ⇒ println("[Error] " + s" $msg in $i")
    }
  }

}