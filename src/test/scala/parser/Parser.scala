package parser

import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.token.StdTokens
import scala.util.parsing.combinator.RegexParsers


trait Directionable

case class Direction(value: String) extends Directionable
case class DirectionDirect(value: Directionable) extends Directionable
case class DirectionIndirect(value: Directionable) extends Directionable

sealed class R(var value: Any = null) extends Directionable

case class R0 extends R
case class R1 extends R
case class R2 extends R
case class R3 extends R
case class R4 extends R

sealed class Instruction 

case class Mov(to: Directionable, from: Directionable) extends Instruction
case class Sub(to: Directionable, from: Directionable) extends Instruction
case class Div(to: Directionable, from: Directionable) extends Instruction
case class Add(to: Directionable, from: Directionable) extends Instruction
case class Jump(to: Directionable) extends Instruction

case class Program(var instructions:List[Instruction])

object QuarqCode {
  val code = """
    main;
		Mov R1, R2;				
	    Sub R2, [[003255]];
	    Add R0, 003255;
	    Div R3, R0;	 
		Jump [00212];
		Jump R3;
  """
  val othercode = """
    MOV R2, 0x0002
    ADD R2, R3
    SUB [0x1000], [R3]
    JUMP [R2], [0x2034]
    """
}

trait QuarqParser extends StdTokenParsers  with ImplicitConversions {
  type Tokens = StdTokens
  val lexical = new StdLexical
  lexical.reserved ++= List("Mov", "R0", "R1", "R2", "R3", "R4", "Sub", "Div", "Add", "Jump",  "main")
  lexical.delimiters ++= List(",", ";", "[",  "]")

  val numberParser: String => Any = { _.toDouble }

  def stringVal = accept("string", { case lexical.StringLit(n) => n })

  def number = accept("number", { case lexical.NumericLit(n) => numberParser(n) })
  
  def registers = "R0" | "R1" | "R2" | "R3" | "R4" 
  
  def register = registers ^^ { case id => Class.forName(s"parser.$id").newInstance().asInstanceOf[R] }
  
  def direction = number ^^ {case direction => Direction(direction.toString)}
  
  def directionDirect = "[" ~> direction <~ "]" ^^ {case direction => DirectionDirect(direction)}
  
  def directionIndirect = "[" ~>directionDirect  <~ "]" ^^ {case direction => DirectionIndirect(direction)}
  
  def directionable = register | directionDirect | directionIndirect
  
  def asignable = directionable | direction

  def instruccions2 = "Mov" | "Sub" | "Div" | "Add"
  
  def instruccions1 = "Jump"  

  def instruction2 = instruccions2 ~ directionable ~ ("," ~> asignable <~";") ^^
    { case ins ~ dir1 ~ dir2 => Class.forName(s"parser.$ins").getConstructor(classOf[Directionable], classOf[Directionable]).newInstance(dir1, dir2).asInstanceOf[Instruction] }

  def instruction1 = instruccions1 ~ (asignable <~";") ^^
    { case ins ~ dir1 => Class.forName(s"parser.$ins").getConstructor(classOf[Directionable]).newInstance(dir1).asInstanceOf[Instruction] }
  
  def program = "main" ~> ";" ~> rep(instruction2 | instruction1) ^^ {case instructions => Program(instructions)}

  def parse(input: String) = phrase(program)(new lexical.Scanner(input))
}

object QuarqExample extends App with QuarqParser {

  val theCode = QuarqCode.code

  parse(theCode) match {
    case Success(result, _) => println(result)
    case Failure(msg, i) => println("[Failure] " + s" $msg in $i")
    case Error(msg, i) => println("[Error] " + s" $msg in $i")
  }
}


