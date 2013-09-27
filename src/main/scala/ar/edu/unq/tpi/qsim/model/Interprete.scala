package ar.edu.unq.tpi.qsim.model
import ar.edu.unq.tpi.qsim.utils.Util
import ar.edu.unq.tpi.qsim.exeptions._

object Interprete {
  
  /**
   * Este mensaje interpreta dos operandos tomando los bits necesarios de la cadena binaria
   * pasada por parametro en forma de String y devuelve un map a op1 y op2 con sus valores
   * respectivos
   * @parameters cadena_binaria : String
   * @return Map[String,ModoDireccionamiento]
   */
  def InterpretarDosOperandos(cadena_binaria : String) : Map[String,ModoDireccionamiento] = {
   
   var bits_segundoOperando : String = ""
   var primer_operando : ModoDireccionamiento = null
   var segundo_operando : ModoDireccionamiento = null
   
   if(cadena_binaria.startsWith("100") || cadena_binaria.startsWith("110"))
   {  primer_operando = interpretar_operando(cadena_binaria)
      bits_segundoOperando = cadena_binaria.takeRight(cadena_binaria.size - 6)   }
   else
   {
     var bits_primerOperando = cadena_binaria.substring(0, 6) + cadena_binaria.substring(12, 29)
     primer_operando = interpretar_operando(bits_primerOperando)
     bits_segundoOperando = cadena_binaria.substring(6, 12) + cadena_binaria.takeRight(16)
   }
   segundo_operando = interpretar_operando(bits_segundoOperando)
    Map(("op1",primer_operando),("op2",segundo_operando))
    }
    
  /**
   * Este mensaje extrae el valor de la cadena binaria pasada por parametro en forma de String 
   * segun el tamanio que tenga y devuelve el valor.
   * respectivos
   * @parameters cadena_binaria : String
   * @return W16
   */
  def extraerValor(cadena_binaria : String) : W16 = {
    
    if(cadena_binaria.size>16) {new W16(Util.binary16ToHex(cadena_binaria.substring(0, 16)))}
    else {new W16(Util.binary16ToHex(cadena_binaria.takeRight(16)))}
  } 
  
  /**
   * Este mensaje interpreta un operando tomando los bits necesarios de la cadena binaria
   * pasada por parametro en forma de String y devuelve un modo de direccionamiento 
   * @parameters cadena_binaria : String
   * @return ModoDireccionamiento
   */
  def interpretar_operando(cadena_binaria : String): ModoDireccionamiento = {

   val code_modo =  cadena_binaria.substring(0, 6)
   var resto =  cadena_binaria.takeRight(cadena_binaria.size - 6) 

   var modo: ModoDireccionamiento = null
    code_modo match {
      case "000000" => Inmediato(extraerValor(resto)) 
      case "001000" => Directo(Inmediato(extraerValor(resto)))
      case "011000" => Indirecto(Directo(Inmediato(extraerValor(resto))))
      case _ => interpretarPosibleRegistro(code_modo)
    }

  }
  
  /**
   * Este mensaje recibe una cadena binaria pasada por parametro en forma de String 
   * y devuelve un jump condicional fijandose segun el comienzo de la cadena a que
   * jump condicional corresponde.
   * @parameters cadena_binaria : String
   * @return Instruccion
   */
   def interpretar_JumpCondicional (cadena_binaria : String): Instruccion = {
    val codOp =  cadena_binaria.substring(0, 4)
    val resto =  cadena_binaria.takeRight(cadena_binaria.size - 4).reverse.takeRight(8).reverse
    codOp match {
      case "0001" => JE(new Salto(resto))
      case "1001" => JNE(new Salto(resto))
      case "0010" => JLE(new Salto(resto))
      case "1010" => JG(new Salto(resto))
      case "0011" => JL(new Salto(resto))
      case "1011" => JGE(new Salto(resto))
      case "0100" => JLEU(new Salto(resto))
      case "1100" => JGU(new Salto(resto))
      case "0101" => JCS(new Salto(resto))
      case "0110" => JNEG(new Salto(resto))
      case "0111" => JVS(new Salto(resto))
      case _ =>  throw new CodigoInvalidoException("Podria haber sido un Jump Condicional pero no coincide con ningun codigo de operacion") 
    }
   }
  
   /**
   * Este mensaje recibe una cadena binaria pasada por parametro en forma de String 
   * y devuelve un JUMP fijandose que tenga el relleno correspondiente.
   * @parameters cadena_binaria : String
   * @return Instruccion
   */
   def interpretar_JMP (cadena_binaria : String): Instruccion = {
    val relleno =  cadena_binaria.substring(0, 6)
    val resto =  cadena_binaria.takeRight(cadena_binaria.size - 6)
    relleno match {
      case "000000" => JMP(interpretar_operando(resto))
      case _ =>  throw new CodigoInvalidoException("Podria haber sido un JMP pero no posee relleno") //ES ERROR PORQUE NO TIENE RELLENO
    }
   }
  
  /**
   * Este mensaje recibe una cadena binaria pasada por parametro en forma de String 
   * y devuelve un NOT fijandose que tenga el relleno correspondiente.
   * @parameters cadena_binaria : String
   * @return Instruccion
   */ 
   def interpretar_NOT (cadena_binaria : String): Instruccion = {
    val relleno =  cadena_binaria.substring(6, 12)
    val resto = cadena_binaria.substring(0, 6) + cadena_binaria.takeRight(cadena_binaria.size - 12)
    relleno match {
      case "000000" => NOT(interpretar_operando(resto))
      case _ =>  throw new CodigoInvalidoException("Podria haber sido un NOT pero no posee relleno") //ES ERROR PORQUE NO TIENE RELLENO
    }

  }
   
   /**
   * Este mensaje recibe una cadena binaria pasada por parametro en forma de String 
   * y devuelve un POP fijandose que tenga el relleno correspondiente.
   * @parameters cadena_binaria : String
   * @return Instruccion
   */ 
   def interpretar_POP (cadena_binaria : String): Instruccion = {
    val relleno =  cadena_binaria.substring(6, 12)
    val resto = cadena_binaria.substring(0, 6) + cadena_binaria.takeRight(cadena_binaria.size - 12)
    relleno match {
      case "000000" => POP(interpretar_operando(resto))
      case _ =>  throw new CodigoInvalidoException("Podria haber sido un NOT pero no posee relleno") //ES ERROR PORQUE NO TIENE RELLENO
    }

  }
  
 /**
   * Este mensaje recibe una cadena binaria pasada por parametro en forma de String 
   * y devuelve un CALL fijandose que tenga el relleno correspondiente.
   * @parameters cadena_binaria : String
   * @return Instruccion
   */  
  def interpretar_CALL (cadena_binaria : String): Instruccion = {
    val relleno =  cadena_binaria.substring(0, 6)
    val resto =  cadena_binaria.takeRight(cadena_binaria.size - 6)
    relleno match {
      case "000000" => CALL(interpretar_operando(resto))
      case _ =>  throw new CodigoInvalidoException("Podria haber sido un CALL pero no posee relleno") //ES ERROR PORQUE NO TIENE RELLENO
    }

  }
  
  /**
   * Este mensaje recibe una cadena binaria pasada por parametro en forma de String 
   * y devuelve un PUSH fijandose que tenga el relleno correspondiente.
   * @parameters cadena_binaria : String
   * @return Instruccion
   */  
  def interpretar_PUSH(cadena_binaria : String): Instruccion = {
    val relleno =  cadena_binaria.substring(0, 6)
    val resto =  cadena_binaria.takeRight(cadena_binaria.size - 6)
    relleno match {
      case "000000" => PUSH(interpretar_operando(resto))
      case _ =>  throw new CodigoInvalidoException("Podria haber sido un PUSH pero no posee relleno") //ES ERROR PORQUE NO TIENE RELLENO
    }

  }
  
  
  def construir_instruccionDosOperandos(constructor:(ModoDireccionamiento,ModoDireccionamiento)=>Instruccion,map : Map[String,ModoDireccionamiento] ) : Instruccion = {
    val op1 = map("op1")
    val op2 = map("op2")
    constructor(op1,op2)    
  }
  
  def interpretarInstruccion(cadena_binaria : String): Instruccion = {
    
    val bits : String =  cadena_binaria.substring(0, 4)
    val resto : String = cadena_binaria.takeRight(cadena_binaria.size - 4) 
    bits match {
      case "1100" => RET()  
      case "1011" => interpretar_CALL(resto)
      case "1010" => interpretar_JMP(resto)
      case "1110" => interpretar_PUSH(resto)
      case "1101" => interpretar_POP(resto)
      case "1001" => interpretar_NOT(resto)
      case "1111" => interpretar_JumpCondicional(resto) 
      case "0001" => construir_instruccionDosOperandos(MOV(_,_),InterpretarDosOperandos(resto)) 
      case "0000" => construir_instruccionDosOperandos(MUL(_,_),InterpretarDosOperandos(resto)) 
      case "0010" => construir_instruccionDosOperandos(ADD(_,_),InterpretarDosOperandos(resto)) 
      case "0011" => construir_instruccionDosOperandos(SUB(_,_),InterpretarDosOperandos(resto)) 
      case "0111" => construir_instruccionDosOperandos(DIV(_,_),InterpretarDosOperandos(resto))
      case "0100" => construir_instruccionDosOperandos(AND(_,_),InterpretarDosOperandos(resto))
      case "0101" => construir_instruccionDosOperandos(OR(_,_),InterpretarDosOperandos(resto))
      case "0110" => construir_instruccionDosOperandos(CMP(_,_),InterpretarDosOperandos(resto))
      case _ => throw new CodigoInvalidoException("No hay ninguna instruccion con ese codigo de operacion")
    }

  }
  
   def interpretarPosibleRegistro(cadena_binaria : String): ModoDireccionamiento = {
   val bits =  cadena_binaria.substring(0, 3)  
   val resto = cadena_binaria.takeRight(3)
    bits match {
      case "100" => interpretarRegistro(resto) 
      case "110" =>  RegistroIndirecto(interpretarRegistro(resto))  
      case _ => throw new CodigoInvalidoException("No hay modos de direccionamiento con ese codigo")
    }

  }
  
  
  def interpretarRegistro(cadena_binaria : String): Registro = {
   
    cadena_binaria match {
      case "000" => R0 
      case "001" => R1 
      case "010" => R2 
      case "011" => R3 
      case "100" => R4 
      case "101" => R5 
      case "110" => R6 
      case "111" => R7 
      case _ => throw new CodigoInvalidoException("No hay modos de direccionamiento con ese codigo")
    }

  }
  

}

object pruebados extends App() {

 
  val d = "100111" + "000000" + "0000000000000001" + "1111110000000001" //R7, 0001
  val b = "100111" + "100011" + "1111000011110000" + "1111111000011110"//R7, R3  
  val a = "100111" + "001000" + "0000000000001111" + "1111000011110000"//R7, [000F]
  val c = "001000" + "100111" + "0000000000001111" + "1111000011110000" // [000F],R7 relleno
  val co= "001000" + "001000" + "0001000111100001" + "1111111111111111"
  val p = "111111" + "011100" + "0011111000000001" + "1111110000000001" //R7, 0001
  val inregistro = "110111" + "110011"
   val registro = "100111" + "100011"
   val directo = "001000" + "0000000000001111"
   val directo_registro = "001000" + "100111" + "0000000000001111" + "1111000011110000" // [000F],R7 

  println("R7, 0001      " + Interprete.InterpretarDosOperandos(d))
  println("R7, R3        " + Interprete.InterpretarDosOperandos(b))
  println("R7, [000F]    " + Interprete.InterpretarDosOperandos(a))
  println("[000F], R7    " + Interprete.InterpretarDosOperandos(c))
  println("[11E1],[FFFF] " + Interprete.InterpretarDosOperandos(co))
  
  println("MUL => " + Interprete.interpretarInstruccion("0000" + d))
  println("ADD => " + Interprete.interpretarInstruccion("0010" + b))
  println("ADD SOLO REGISTROS=> " + Interprete.interpretarInstruccion("0010" + registro))
  println("ADD SOLO REGISTROS INDIRECTOS=> " + Interprete.interpretarInstruccion("0010" + inregistro))
  println("ADD directo registro=> " + Interprete.interpretarInstruccion("0010" + directo_registro))
  println("DIV => " + Interprete.interpretarInstruccion("0111" + a))
  println("MOV => " + Interprete.interpretarInstruccion("0001" + c))
  println("SUB => " + Interprete.interpretarInstruccion("0011" + co))
  println("CALL => " + Interprete.interpretarInstruccion("1011" + "000000" + d))
  println("JMP => " + Interprete.interpretarInstruccion("1010" + "000000" + d))
  println("JMP REGISTRO => " + Interprete.interpretarInstruccion("1010" + "000000" + "100111"))
  println("JMP DIRECTO => " + Interprete.interpretarInstruccion("1010" + "000000" + directo))
  println("JE => " + Interprete.interpretarInstruccion("1111" + "0001" + "00000000" ))
  println("JG => " + Interprete.interpretarInstruccion("1111" + "1010" + "00000000" + "1111000011110000" +  "1111000011110000"))
  println("JLE => " + Interprete.interpretarInstruccion("1111" + "0010" + "00000000" +  "1111000011110000"))
  println("JL => " + Interprete.interpretarInstruccion("1111" + "0011" + "00000000" + "1111000011110000" +  "1111000011110000"))
  println("JGE => " + Interprete.interpretarInstruccion("1111" + "1011" + "00000000" + "1111000011110000" +  "1111000011110000"))
  println("JLEU => " + Interprete.interpretarInstruccion("1111" + "0100" + "00000000" + "1111000011110000" +  "1111000011110000"))
  println("JGU => " + Interprete.interpretarInstruccion("1111" + "1100" + "00000000" + "1111000011110000" +  "1111000011110000"))
  println("JCS => " + Interprete.interpretarInstruccion("1111" + "0101" + "00000000" + "1111000011110000" +  "1111000011110000"))
  println("JNEG => " + Interprete.interpretarInstruccion("1111" + "0110" + "10000000" + "1111000011110000" +  "1111000011110000"))
  println("JVS => " + Interprete.interpretarInstruccion("1111" + "0111" + "00000011" + "1111000011110000" +  "1111000011110000"))
  println("JNE => " + Interprete.interpretarInstruccion("1111" + "1001" + "00110000" + "1111000011110000" +  "1111000011110000"))
 

  println("RET => " + Interprete.interpretarInstruccion("1100" + d))
  println("NOT => " + Interprete.interpretarInstruccion("1001" + "110110" +"000000" + "1111111111111111"))
  println("NOT => " + Interprete.interpretarInstruccion("1001" + "110110" +"000000" + "1111111111111111"))
 
  
  // println("PORQUERIA => " + Ensamblador.ensamblarInstruccion("1111" + p))
  //println("PORQUERIA DOS => " + Ensamblador.ensamblarInstruccion("0001" + p))

  
}