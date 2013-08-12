package ar.edu.unq.tpi.qsim.parser

import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.token.StdTokens
import scala.util.parsing.combinator.lexical.StdLexical
import ar.edu.unq.tpi.qsim.beans._
import scala.collection.mutable.ArrayBuffer

trait ArchitecturesQParser extends StdTokenParsers  with ImplicitConversions {
  type Tokens = StdTokens
  val lexical = new StdLexical
  lexical.reserved ++= List("MOV", "SUB", "ADD", "DIV", "MUL", "R0", "R1", "R2", "R3", "R4", "R5", "R6", "R7")
  lexical.delimiters ++= List(",", ";", "[",  "]")
  
  val numberParser: String => Any = { _.toDouble }

  def stringVal = accept("string", { case lexical.StringLit(n) => n })

  def number = accept("number", { case lexical.NumericLit(n) => numberParser(n) })
  
  def registers = "R0" ^^^ R0 | 
  				  "R1" ^^^ R1 | 
  				  "R2" ^^^ R2 |
  				  "R3" ^^^ R3 | 
  				  "R4" ^^^ R4 |
  				  "R5" ^^^ R5 | 
  				  "R6" ^^^ R6 | 
  				  "R7" ^^^ R7
  
  def register = registers //^^ { case id => Class.forName(s"ar.edu.unq.tpi.qsim.parser.$id").newInstance().asInstanceOf[R] }
  
  def inmediate = number ^^ {case direction => Immediate(direction.toString)}
  
  //def directionDirect = "[" ~> direction <~ "]" ^^ {case direction => DirectionDirect(direction)}
  
  //def directionIndirect = "[" ~>directionDirect  <~ "]" ^^ {case direction => DirectionIndirect(direction)}
  
  def directionable = register | inmediate //directionDirect | directionIndirect
  
  def asignable = register

  def instruccions2 = "MOV" | "SUB" | "DIV" | "ADD" | "MUL"
  
  //def instruccions1 = "Jump"  

  def instruction2 = instruccions2 ~ asignable ~ ("," ~> directionable <~";") ^^
    { case ins ~ dir1 ~ dir2 => Class.forName(s"ar.edu.unq.tpi.qsim.beans.$ins").getConstructor(classOf[String],classOf[String]).newInstance(dir1.toString, dir2.toString()).asInstanceOf[Instruction] }

//  def instruction1 = instruccions1 ~ (asignable <~";") ^^
//    { case ins ~ dir1 => Class.forName(s"parser.$ins").getConstructor(classOf[Directionable]).newInstance(dir1).asInstanceOf[Instruction] }
  
  def program = rep(instruction2) ^^ {case instructions => Program(ArrayBuffer()++instructions)}

  def parse(input: String) = phrase(program)(new lexical.Scanner(input))
}

object QuarqExample extends App with ArchitecturesQParser {

  val theCode = """
		MOV R1, R2;				
	    SUB R2, 0023;
	    ADD R0, 0255;
	    DIV R3, R0;	 
    """

  parse(theCode) match {
    case Success(result, _) => println(result)
    case Failure(msg, i) => println("[Failure] " + s" $msg in $i")
    case Error(msg, i) => println("[Error] " + s" $msg in $i")
  }
}