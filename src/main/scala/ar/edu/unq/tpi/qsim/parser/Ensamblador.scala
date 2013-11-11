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
   * Estos son los 7 registros que se van a usar para el operando destino de la instrucion MUL.
   * Lo que hace este parser es reemplazar el valor del (string) por el objeto (registro) correspondiente.
   */  	 
  def registersMUL = "R0" ^^^ R0 | "R1" ^^^ R1 | "R2" ^^^ R2 | "R3" ^^^ R3 |
    "R4" ^^^ R4 | "R5" ^^^ R5 | "R6" ^^^ R6
  
    /**
   * Defino el parser del modo de direccionamiento registro para la instruccion MUL.
   */
  def registerMUL = registersMUL

  /**
   * Este parser ensambla el modo de direccionamiento registro indirecto.
   * Lo que hace es tomar lo que realmente importa que es el contenido del (registro)
   */
  def registerIndirect = "[" ~> register <~ "]" ^^ { case register ⇒ RegistroIndirecto(register) }

  /**
   * Este parser ensambla el modo de direccionamiento inmediato.
   * Toma el valor del inmediato.
   */
  def inmediate = "0x" ~> "[0-9A-F]{4}".r ^^ { case direction ⇒ Inmediato(new W16(direction)) }

  /**
   * Este parser ensambla el modo de direcciomaiento directo.
   * Toma el inmediato para poder crear un Directo.
   */
  def direct = "[" ~> inmediate <~ "]" ^^ { case direction ⇒ Directo(direction) }

  /**
   * Este parser ensambla el modo de direccionamiento Indirecto.
   * Toma el directo para cear un indirecto.
   */
  def indirect = "[" ~> direct <~ "]" ^^ { case direction ⇒ Indirecto(direction) }

  /**
   * Este parser ensambla una etiqueta.
   * Toma el nombre de la etiqueta y crea un Modo de direccionamiento Etiqueta.
   */
  def etiqueta = ident ^^ { case etiqueta ⇒ Etiqueta(etiqueta) }

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
   *  Este parser indica que parser se van a usar para ensamblar el operando destino de la instruccion MUL de Q1
   */
  def asignableMULQ1 = registerMUL 
  
  /**
   *  Estas son las instrucciones validas en Q1
   */
  def instruccionsQ1DosOperandos = "MOV" | "SUB" | "DIV" | "ADD" 
   
  /**
   *  Esta es la instruccion MUL de Q1
   */
  def instrucionMULQ1 = "MUL"
  /**
   * Este parser ensambla las instrucciones de Q1
   * Toma la instruccion, el operando1 y el operando2 para poder crear una Instruccion en Q1.
   */
  def instruction2Q1 = instruccionesDosOperandos(instruccionsQ1DosOperandos, asignableQ1, directionableQ1)
  
  /**
   * Este parser ensambla la instruccion MUL de Q1
   * Toma la instruccion, el operando1 y el operando2 para poder crear una Instruccion en Q1.
   */
  def instructionMUL2Q1 = instruccionesDosOperandos(instrucionMULQ1, asignableMULQ1, directionableQ1)
  
  def instructionQ1 = instruction2Q1 | instructionMUL2Q1 

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
   * Este parser indica que parser se van a usar para ensamblar el operando destino de la instruccion MUL de Q2
   */
  def asignableMULQ2 = asignableMULQ1 | direct

  /**
   * Este parser ensambla las instrucciones de Q2
   * Toma la instruccion, el operando1 y el operando2 para poder crear una Instruccion en Q2.
   */
  def instruction2Q2 = instruccionesDosOperandos(instruccionsQ1DosOperandos, asignableQ2, directionableQ2)
  
   /**
   * Este parser ensambla la instruccion MUL de Q2
   * Toma la instruccion, el operando1 y el operando2 para poder crear una Instruccion en Q2.
   */
  def instructionMUL2Q2 = instruccionesDosOperandos(instrucionMULQ1, asignableMULQ2, directionableQ2)
  
  def instructionQ2 = instruction2Q2 | instructionMUL2Q2
  
  ///////////////////////////////////////////////////////////////////////////////////

  // PARSER ENSAMBLADOR - Q3

  /**
   * Este parser indica que parser se van a usar para ensamblar el operando origen de la instruccion CALL y MOV de Q3
   */
  def directionable1Q3 = directionableQ2 | etiqueta

  /**
   *  Estas son las instrucciones validas en Q3
   */
  def instruccionsQ3DosOperandos = "SUB" | "DIV" | "ADD"
    
  /**
   * Se agrega la instruccion MOV con etiqueta al conjunto de instrucciones de Q3
   */
  def movQ3DosOperandos = "MOV"

  /**
   * Se agrega la instruccion CALL al conjunto de instrucciones de Q3
   */
  def instruccionsQ3UnOperando = "CALL"

  /**
   * Se agrega la instruccion RET al conjunto de instrucciones de Q3
   */
  def instruccionsQ3SinOperando = "RET"

  /**
   * Este parser ensambla las instrucciones de Q3
   * Toma la instruccion, el operando1 y el operando2 para poder crear una Instruccion en Q3.
   */
  def instruction2Q3 = instruccionesDosOperandos(instruccionsQ3DosOperandos, asignableQ2, directionableQ2)
  
  /**
   * Este parser ensambla la instruccion MUL de Q3
   * Toma la instruccion, el operando1 y el operando2 para poder crear una Instruccion en Q3.
   */
  def instructionMUL2Q3 = instruccionesDosOperandos(instrucionMULQ1, asignableMULQ2, directionableQ2)
  /**
   * Este parser ensambla las instrucciones de un operando en Q3
   * Toma la instruccion y operando origen y crea una una Instruccion de Un operando en Q3
   */
  def instruction1Q3 = instruccionsQ3UnOperando ~ directionable1Q3 ^^
    { case ins ~ dir2 ⇒ Class.forName(s"ar.edu.unq.tpi.qsim.model.$ins").getConstructor(classOf[ModoDireccionamiento]).newInstance(dir2).asInstanceOf[Instruccion_UnOperando] }

  /**
   * Este parser ensambla la instruccion MOV de dos operandos en Q3
   * Toma la instruccion y operando origen y crea una una Instruccion MOV en Q3
   */
  def instruction2movQ3 = instruccionesDosOperandos(movQ3DosOperandos, asignableQ2, directionable1Q3)

  /**
   * Este parser ensambla las instrucciones sin operandos en Q3
   * Toma la instruccion y crea una una Instruccion Sin Operandos en Q3
   */
  def instruction0Q3 = instruccionsQ3SinOperando ^^
    { case ins ⇒ Class.forName(s"ar.edu.unq.tpi.qsim.model.$ins").getConstructor().newInstance().asInstanceOf[Instruccion_SinOperandos] }

  def instructionQ3 = instruction2Q3 | instruction1Q3 | instruction2movQ3 | instruction0Q3 | instructionMUL2Q3

  //TODO HACER QUE MUL CON R7 como DESTINO NO SE PERMITA

  ///////////////////////////////////////////////////////////////////////////////////

  // PARSER ENSAMBLADOR - Q4

  /**
   *  Estas son las instrucciones validas en Q4
   */
  def instruccionsQ4DosOperandos = instruccionsQ3DosOperandos | "CMP"

  /**
   * Se agrega la instruccion JMP al conjunto de instrucciones de Q4
   */
  def instruccionsQ4UnOperando = instruccionsQ3UnOperando | "JMP"

  /**
   * Se agregan los saltos al conjunto de instrucciones de Q4
   */
  def instruccionsQ4Saltos = "JE" | "JNE" | "JLE" | "JG" | "JL" | "JGE" | "JLEU" | "JGU" | "JCS" | "JNEG" | "JVS"

  /**
   * Este parser ensambla los saltos en Q4.
   * Toma la instruccion y la etiqueta y crea un Salto en Q4.
   */
  def instructionQ4Saltos = instruccionsQ4Saltos ~ etiqueta ^^
    { case ins ~ desp ⇒ Class.forName(s"ar.edu.unq.tpi.qsim.model.$ins").getConstructor(classOf[Salto]).newInstance(new SaltoEtiqueta(desp)).asInstanceOf[JUMP_condicional] }

  /**
   * Este parser ensambla las instrucciones de un operando en Q4
   * Toma la instruccion y operando origen y crea una una Instruccion de Un operando en Q4
   */
  def instruction1Q4 = instruccionsQ4UnOperando ~ directionable1Q3 ^^
    { case ins ~ dir2 ⇒ Class.forName(s"ar.edu.unq.tpi.qsim.model.$ins").getConstructor(classOf[ModoDireccionamiento]).newInstance(dir2).asInstanceOf[Instruccion_UnOperando] }

  /**
   * Este parser ensambla las instrucciones de Q4
   * Toma la instruccion, el operando1 y el operando2 para poder crear una Instruccion en Q4.
   */
  def instruction2Q4 = instruccionesDosOperandos(instruccionsQ4DosOperandos, asignableQ2, directionableQ2)

  def instructionQ4 = instruction2Q4 | instruction1Q4 | instructionQ4Saltos | instruction2movQ3 | instruction0Q3 | instructionMUL2Q3
  

  ///////////////////////////////////////////////////////////////////////////////////

  // PARSER ENSAMBLADOR - Q5

  /**
   * Este parser indica que parser se van a usar para ensamblar el operando destino de las instrucciones de Q5
   */
  def asignableQ5 = asignableQ2 | indirect | registerIndirect

  /**
   * Este parser indica que parser se van a usar para ensamblar el operando destino de la instruccion MUL de Q5
   */
  def asignableMULQ5 = asignableMULQ2 | indirect | registerIndirect

  /**
   * Este parser indica que parser se van a usar para ensamblar el operando destino de las instrucciones de Q5
   */
  def directionableQ5 = directionableQ2 | indirect | registerIndirect

  /**
   * Este parser indica que parser se van a usar para ensamblar el operando origen de la instruccion CALL , JMP y MOV de Q5
   */
  def directionable1Q5 = directionable1Q3 | indirect | registerIndirect

  /**
   * Este parser ensambla la instruccion MOV de dos operandos en Q5
   * Toma la instruccion y operando origen y crea una una Instruccion MOV en Q5
   */
  def instruction2movQ5 = instruccionesDosOperandos(movQ3DosOperandos, asignableQ5, directionable1Q5)

  /**
   * Este parser ensambla las instrucciones de un operando en Q5
   * Toma la instruccion y operando origen y crea una una Instruccion de Un operando en Q5
   */
  def instruction1Q5 = instruccionsQ4UnOperando ~ directionable1Q5 ^^
    { case ins ~ dir2 ⇒ Class.forName(s"ar.edu.unq.tpi.qsim.model.$ins").getConstructor(classOf[ModoDireccionamiento]).newInstance(dir2).asInstanceOf[Instruccion_UnOperando] }

  /**
   * Este parser ensambla las instrucciones de Q5
   * Toma la instruccion, el operando1 y el operando2 para poder crear una Instruccion en Q5.
   */
  def instructionMUL2Q5 = instruccionesDosOperandos(instrucionMULQ1, asignableMULQ5, directionableQ5)

  /**
   * Este parser ensambla las instrucciones de Q5
   * Toma la instruccion, el operando1 y el operando2 para poder crear una Instruccion en Q5.
   */
  def instruction2Q5 = instruccionesDosOperandos(instruccionsQ4DosOperandos, asignableQ5, directionableQ5)

  def instructionQ5 = instructionQ4Saltos | instruction2movQ5 | instruction1Q5 | instruction2Q5 | instruction0Q3 | instructionMUL2Q5

  ///////////////////////////////////////////////////////////////////////////////////

  // PARSER ENSAMBLADOR - Q6

  /**
   *  Estas son las instrucciones validas en Q6
   */
  def instruccionsQ6DosOperandos = instruccionsQ4DosOperandos | "AND" | "OR"

  /**
   * Se agrega la instruccion NOT al conjunto de instrucciones de Q6
   */
  def instruccionsQ6UnOperando = "NOT"

  /**
   * Este parser ensambla las instrucciones de un operando en Q6
   * Toma la instruccion y operando destino y crea una una Instruccion de Un operando en Q6
   */
  def instruction1Q6 = instruccionsQ6UnOperando ~ asignableQ5 ^^
    { case ins ~ dir2 ⇒ Class.forName(s"ar.edu.unq.tpi.qsim.model.$ins").getConstructor(classOf[ModoDireccionamiento]).newInstance(dir2).asInstanceOf[Instruccion_UnOperando] }

  /**
   * Este parser ensambla las instrucciones de Q6
   * Toma la instruccion, el operando1 y el operando2 para poder crear una Instruccion en Q6.
   */
  def instruction2Q6 = instruccionesDosOperandos(instruccionsQ6DosOperandos, asignableQ5, directionableQ5)

  def instructionQ6 = instructionQ4Saltos | instruction2movQ5 | instruction1Q5 | instruction1Q6 | instruction2Q6 | instruction0Q3 | instructionMUL2Q5
//--------------------------------------------------------------------------------------------
  // Construccion de Instrucciones de Dos Operandos !!!
  def instruccionesDosOperandos(instrucciones: Parser[String], asignable: Parser[ModoDireccionamiento], direccionable: Parser[ModoDireccionamiento]) = instrucciones ~ asignable ~ ("," ~> direccionable) ^^
    { case ins ~ dir1 ~ dir2 ⇒ Class.forName(s"ar.edu.unq.tpi.qsim.model.$ins").getConstructor(classOf[ModoDireccionamiento], classOf[ModoDireccionamiento]).newInstance(dir1, dir2).asInstanceOf[Instruccion_DosOperandos] }

  // Agregar las instrucciones de cada Q:
  def instructionsQ1 = instructionQ1
  def instructionsQ2 = instructionQ2

  // Agregar a partor de Q3 la idea de etiqueta :
  def instructionsQ3 = ((ident <~ ":")?) ~ instructionQ3
  def instructionsQ4 = ((ident <~ ":")?) ~ instructionQ4
  def instructionsQ5 = ((ident <~ ":")?) ~ instructionQ5
  def instructionsQ6 = ((ident <~ ":")?) ~ instructionQ6

  // Especificar cada Programa
  def programQ1 = programSE(instructionsQ1)
  def programQ2 = programSE(instructionsQ2)
  def programQ3 = program(instructionsQ3)
  def programQ4 = program(instructionsQ4)
  def programQ5 = program(instructionsQ5)
  def programQ6 = program(instructionsQ6)

  // Construccion de un Programa!!!
  def program(parser: Parser[Option[String] ~ Instruccion]) = rep(parser) ^^
    { case instructions ⇒ Programa(instructions.map(p ⇒ (p._1, p._2))) }

  // Construccion de un Programa Sin Etiquetas!!!
  def programSE(parser: Parser[Instruccion]) = rep(parser) ^^
    { case instructions ⇒ Programa(instructions.map(p ⇒ (None, p))) }

  // Especificacion del Parser
  def parse(input: String, parserQ: Parser[Programa]) = parseAll(parserQ, input)
}

object lala extends App {

  println(Parser.parse("""MUL R7, R7""", Parser.programQ6))

} 
