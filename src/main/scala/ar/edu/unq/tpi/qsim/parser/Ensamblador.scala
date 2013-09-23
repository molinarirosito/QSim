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
import scala.io.Source
// TODO Comentar a Tati sobre el Error y si le parece correcto o no dejarlo asi !! o restringir que toma solamente 4 digitos hexa
trait Ensamblador extends JavaTokenParsers with ImplicitConversions {
  type Tokens = StdTokens
  val lexical = new StdLexical
  lexical.reserved ++= List("MOV", "SUB", "ADD", "DIV", "MUL", "R0", "R1", "R2", "R3", "R4", "R5", "R6", "R7")
  lexical.delimiters ++= List(",", "[", "]", "0x", ":")
  
  // PARSER DE LOS MODOS DE DIRECCIONAMIENTO
  /**
   * Estos son los 8 registros que se van a usar.
   * Lo que hace este parser es reemplazar el valor del (string) por el objeto (registro) correspondiente.
   */
  def registers = "R0" ^^^ R0 | "R1" ^^^ R1 | "R2" ^^^ R2 | "R3" ^^^ R3 |
    			  "R4" ^^^ R4 | "R5" ^^^ R5 | "R6" ^^^ R6 | "R7" ^^^ R7
  /**
   * Defino el parser del modo de direccionamiento registro.
   */
  def register = registers
  
  /**
   * Este parser ensambla el modo de direccionamiento registro indirecto.
   * Lo que hace es tomar lo que realmente importa que es el contenido del (registro)
   */
  def registerIndirect = "[" ~> register <~ "]" ^^ { case register => RegistroIndirecto(register) }
  
  /**
   * Este parser ensambla el modo de direccionamiento inmediato.
   * Toma el valor del inmediato.
   */
  def inmediate = "0x" ~> "[0-9A-Z]+".r ^^ { case direction => Inmediato(new W16(direction)) }
  
  /**
   * Este parser ensambla el modo de direcciomaiento directo.
   * Toma el inmediato para poder crear un Directo. 
   */
  def direct = "[" ~> inmediate <~ "]" ^^ { case direction => Directo(direction) }
  
  /**
   * Este parser ensambla el modo de direccionamiento Indirecto.
   * Toma el directo para cear un indirecto.
   */
  def indirect = "[" ~> direct <~ "]" ^^ {case direction => Indirecto(direction)}
  
  /**
   * Este parser ensambla una etiqueta. 
   * Toma el nombre de la etiqueta y crea un Modo de direccionamiento Etiqueta. 
   */
  def etiqueta = ident ^^ {case etiqueta => Etiqueta(etiqueta)}
  
  // PARSER ENSAMBLADOR - Q1
  
  /**
   *  Este parser indica que parser se van a usar para ensamblar el operando origen de las instrucciones de Q1  
   */ 
  def directionableQ1 = register | inmediate
  
  /**
   *  Este parser indica que parser se van a usar para ensamblar el operando destino de las instrucciones de Q1  
   */ 
  def asignableQ1 = register
  
  /**
   *  Estas son las instrucciones validas en Q1   
   */ 
  def instruccionsQ1DosOperandos = "MOV" | "SUB" | "DIV" | "ADD" | "MUL"
  
  /**
   * Este parser ensambla las instrucciones de Q1
   * Toma la instruccion, el operando1 y el operando2 para poder crear una Instruccion en Q1.
   */
  def instruction2Q1 = instruccionsQ1DosOperandos ~ asignableQ1 ~ ("," ~> directionableQ1) ^^
  { case ins ~ dir1 ~ dir2 => Class.forName(s"ar.edu.unq.tpi.qsim.model.$ins").getConstructor(classOf[ModoDireccionamiento], classOf[ModoDireccionamiento]).newInstance(dir1, dir2).asInstanceOf[Instruccion_DosOperandos] }

////////////////////////////////////////////////////////////////////////////////////////
  
  // PARSER ENSAMBLADOR - Q2
  
  /**
   * Este parser indica que parser se van a usar para ensamblar el operando origen de las instrucciones de Q2
   */
  def directionableQ2 = directionableQ1 | direct

  /**
   * Este parser indica que parser se van a usar para ensamblar el operando destino de las instrucciones de Q2
   */
  def asignableQ2 = asignableQ1 | direct

  /**
   * Este parser ensambla las instrucciones de Q2
   * Toma la instruccion, el operando1 y el operando2 para poder crear una Instruccion en Q2.
   */
  def instruction2Q2 = instruccionsQ1DosOperandos ~ asignableQ2 ~ ("," ~> directionableQ2) ^^
  { case ins ~ dir1 ~ dir2 => Class.forName(s"ar.edu.unq.tpi.qsim.model.$ins").getConstructor(classOf[ModoDireccionamiento], classOf[ModoDireccionamiento]).newInstance(dir1, dir2).asInstanceOf[Instruccion_DosOperandos] }
  
///////////////////////////////////////////////////////////////////////////////////
  
  // PARSER ENSAMBLADOR - Q3
 
  /**
   * Se agrega la instruccion CALL al conjunto de instrucciones de Q3
   */
  def instruccionsQ3UnOperando = "CALL" 
  
  /**
   * Se agrega la instruccion RET al conjunto de instrucciones de Q3
   */
  def instruccionsQ3SinOperando = "RET"
  
  /** 
   * Este parser ensambla las instrucciones de un operando en Q3
   * Toma la instruccion y operando origen y crea una una Instruccion de Un operando en Q3
   */
    // TODO esto no esta bien, tiene que permitirse que sea un modo de direccionamiento y un etiqueta a la vez el operando origen
  def instruction1Q3 = instruccionsQ3UnOperando ~ etiqueta ^^
    { case ins ~ etiq => Class.forName(s"ar.edu.unq.tpi.qsim.model.$ins").getConstructor(classOf[ModoDireccionamiento]).newInstance(etiq).asInstanceOf[Instruccion_UnOperando] }
  
  /**
   * Este parser ensambla las instrucciones sin operandos en Q3
   * Toma la instruccion y crea una una Instruccion Sin Operandos en Q3
   */
  def instruction0Q3 = instruccionsQ3SinOperando ^^
    { case ins => Class.forName(s"ar.edu.unq.tpi.qsim.model.$ins").getConstructor().newInstance().asInstanceOf[Instruccion_SinOperandos] }
  
 // def instruction(parser: Parser[ModoDireccionamiento) = rep(parser) ^^
   // { case instructions => Programa(instructions.map(p => (p._1, p._2))) }

  def instructionsQ1 = ((ident <~ ":")?) ~ instruction2Q1
 
  def instructionsQ2 = ((ident <~ ":")?) ~ instruction2Q2

 // def instructionsQ3 = ((ident <~ ":")?) ~ instruction2Q2 | instruction0Q3 | instruction1Q3
  
  def instructionsQ3 = ((ident <~ ":")?) ~ instruction1Q3 	
  
  def programQ1 = program(instructionsQ1)
  def programQ2 = program(instructionsQ2)
  def programQ3 = program(instructionsQ3)

  def program(parser: Parser[Option[String] ~ Instruccion]) = rep(parser) ^^
    { case instructions => Programa(instructions.map(p => (p._1, p._2))) }

  // def program = programQ1 | programQ2 | programQ3

  def parse(input: String) = parseAll(programQ1, input)
}