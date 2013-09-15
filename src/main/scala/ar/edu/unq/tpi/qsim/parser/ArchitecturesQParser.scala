package ar.edu.unq.tpi.qsim.parser

import scala.util.matching.Regex
import scala.util.parsing.combinator._
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.combinator.token.StdTokens
import scala.util.parsing.input._
import ar.edu.unq.tpi.qsim.model._
import scala.collection.mutable.ArrayBuffer
import ar.edu.unq.tpi.qsim.model.Programa

trait ArchitecturesQParser extends JavaTokenParsers with ImplicitConversions {
  type Tokens = StdTokens
  val lexical = new StdLexical
  lexical.reserved ++= List("MOV", "SUB", "ADD", "DIV", "MUL", "R0", "R1", "R2", "R3", "R4", "R5", "R6", "R7")
  lexical.delimiters ++= List(",", "[", "]", "0x", ":")

  def registers = "R0" ^^^ R0 |
    "R1" ^^^ R1 |
    "R2" ^^^ R2 |
    "R3" ^^^ R3 |
    "R4" ^^^ R4 |
    "R5" ^^^ R5 |
    "R6" ^^^ R6 |
    "R7" ^^^ R7

  def register = registers

  def inmediate = "0x" ~> "[0-9A-Z]+".r ^^ { case direction ⇒ Inmediato(new W16(direction)) }

  def direct = "[" ~> inmediate <~ "]" ^^ { case direction ⇒ Directo(direction) }

  //def directionIndirect = "[" ~>directionDirect  <~ "]" ^^ {case direction => DirectionIndirect(direction)}

  def directionableQ1 = register | inmediate

  def directionableQ2 = directionableQ1 | direct

  def asignableQ1 = register

  def asignableQ2 = asignableQ1 | direct

  //operaciones
  def instruccions2 = "MOV" | "SUB" | "DIV" | "ADD" | "MUL"

  def instruccions1 = "CALL"

  def instruccions0 = "RET"

  def instruction2Q1 = instruccions2 ~ asignableQ1 ~ ("," ~> directionableQ1) ^^
    { case ins ~ dir1 ~ dir2 ⇒ Class.forName(s"ar.edu.unq.tpi.qsim.model.$ins").getConstructor(classOf[ModoDireccionamiento], classOf[ModoDireccionamiento]).newInstance(dir1, dir2).asInstanceOf[Instruccion_DosOperandos] }

  def instruction2Q2 = instruccions2 ~ asignableQ2 ~ ("," ~> directionableQ2) ^^
    { case ins ~ dir1 ~ dir2 ⇒ Class.forName(s"ar.edu.unq.tpi.qsim.model.$ins").getConstructor(classOf[ModoDireccionamiento], classOf[ModoDireccionamiento]).newInstance(dir1, dir2).asInstanceOf[Instruccion_DosOperandos] }

  def instruction1Q3 = instruccions1 ~ asignableQ2 ^^
    { case ins ~ dir1 ⇒ Class.forName(s"ar.edu.unq.tpi.qsim.model.$ins").getConstructor(classOf[ModoDireccionamiento]).newInstance(dir1).asInstanceOf[Instruccion_UnOperando] }

  def instruction0Q3 = instruccions0 ^^
    { case ins ⇒ Class.forName(s"ar.edu.unq.tpi.qsim.model.$ins").getConstructor().newInstance().asInstanceOf[Instruccion_SinOperandos] }

  def instructionsQ1 = ((ident <~ ":")?) ~ instruction2Q1
 
  def instructionsQ2 = ((ident <~ ":")?) ~ instruction2Q2

  def instructionsQ3 = ((ident <~ ":")?) ~ instruction2Q2 | instruction0Q3 | instruction1Q3

  def programQ1 = program(instructionsQ1)
  def programQ2 = program(instructionsQ2)
  //  def programQ3 = program(instructionsQ3)

  def program(parser: Parser[Option[String] ~ Instruccion]) = rep(parser) ^^
    { case instructions ⇒ Programa(instructions.map(p ⇒ (p._1, p._2))) }

  // def program = programQ1 | programQ2 | programQ3

  def parse(input: String) = parseAll(programQ1, input)
}

object QuarqExample extends App with ArchitecturesQParser {

  val input = io.Source.fromFile("src/main/resources/programa.qsim")
  val str = input.mkString

  parse(str) match {
    case Success(result, _) ⇒ 
    println(result)
    case Failure(msg, i) ⇒ println("[Failure] " + s" $msg in $i")
    case Error(msg, i) ⇒ println("[Error] " + s" $msg in $i")
  }
}
