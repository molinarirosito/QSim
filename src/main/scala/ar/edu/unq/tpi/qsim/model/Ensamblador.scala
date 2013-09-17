package ar.edu.unq.tpi.qsim.model
import ar.edu.unq.tpi.qsim.utils.Util
import ar.edu.unq.tpi.qsim.exeptions._

object Ensamblador {
  
  
  def ensamblarDosOperandos(cadena_binaria : String) : Map[String,ModoDireccionamiento] = {
   
   var bits_segundoOperando : String = ""
   var primer_operando : ModoDireccionamiento = null
   var segundo_operando : ModoDireccionamiento = null
   
   if(cadena_binaria.startsWith("100") || cadena_binaria.startsWith("110"))
   {  primer_operando = ensamblar_operando(cadena_binaria)
      bits_segundoOperando = cadena_binaria.takeRight(cadena_binaria.size - 6)   }
   else
   {
     var bits_primerOperando = cadena_binaria.substring(0, 6) + cadena_binaria.substring(12, 29)
     primer_operando = ensamblar_operando(bits_primerOperando)
     bits_segundoOperando = cadena_binaria.substring(6, 12) + cadena_binaria.takeRight(16)
   }
   segundo_operando = ensamblar_operando(bits_segundoOperando)
    Map(("op1",primer_operando),("op2",segundo_operando))
    }
    
  def extraerValor(cadena_binaria : String) : W16 = {
    
    if(cadena_binaria.size>16) {new W16(Util.binary16ToHex(cadena_binaria.substring(0, 16)))}
    else {new W16(Util.binary16ToHex(cadena_binaria.takeRight(16)))}
  } 
  
  def ensamblar_operando(cadena_binaria : String): ModoDireccionamiento = {

   val code_modo =  cadena_binaria.substring(0, 6)
   var resto =  cadena_binaria.takeRight(cadena_binaria.size - 6) 

   var modo: ModoDireccionamiento = null
    code_modo match {
      case "000000" => Inmediato(extraerValor(resto)) 
      case "001000" => Directo(Inmediato(extraerValor(resto)))
      case "011000" => Indirecto(Directo(Inmediato(extraerValor(resto))))
      case _ => ensamblarPosibleRegistro(code_modo)
    }

  }
  
   def ensamblar_JumpCondicional (cadena_binaria : String): Instruccion = {
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
      case _ =>  throw new InvalidCodeException("Podria haber sido un Jump Condicional pero no coincide con ningun codigo de operacion") 
    }
   }
  
   def ensamblar_JMP (cadena_binaria : String): Instruccion = {
    val relleno =  cadena_binaria.substring(0, 6)
    val resto =  cadena_binaria.takeRight(cadena_binaria.size - 6)
    relleno match {
      case "000000" => JMP(ensamblar_operando(resto))
      case _ =>  throw new InvalidCodeException("Podria haber sido un JMP pero no posee relleno") //ES ERROR PORQUE NO TIENE RELLENO
    }
   }
  
   def ensamblar_NOT (cadena_binaria : String): Instruccion = {
    val relleno =  cadena_binaria.substring(0, 6)
    val resto =  cadena_binaria.takeRight(cadena_binaria.size - 6)
    relleno match {
      case "000000" => NOT(ensamblar_operando(resto))
      case _ =>  throw new InvalidCodeException("Podria haber sido un NOT pero no posee relleno") //ES ERROR PORQUE NO TIENE RELLENO
    }

  }
   
  def ensamblar_CALL (cadena_binaria : String): Instruccion = {
    val relleno =  cadena_binaria.substring(0, 6)
    val resto =  cadena_binaria.takeRight(cadena_binaria.size - 6)
    relleno match {
      case "000000" => CALL(ensamblar_operando(resto))
      case _ =>  throw new InvalidCodeException("Podria haber sido un CALL pero no posee relleno") //ES ERROR PORQUE NO TIENE RELLENO
    }

  }
  
  def construir_instruccionDosOperandos(constructor:(ModoDireccionamiento,ModoDireccionamiento)=>Instruccion,map : Map[String,ModoDireccionamiento] ) : Instruccion = {
    val op1 = map("op1")
    val op2 = map("op2")
    constructor(op1,op2)    
  }
  
  def ensamblarInstruccion(cadena_binaria : String): Instruccion = {
    
    val bits : String =  cadena_binaria.substring(0, 4)
    val resto : String = cadena_binaria.takeRight(cadena_binaria.size - 4) 
    bits match {
      case "1100" => RET()  
      case "1011" => ensamblar_CALL(resto)
      case "1010" => ensamblar_JMP(resto)
      case "1001" => ensamblar_NOT(resto)
      case "1111" => ensamblar_JumpCondicional(resto) 
      case "0001" => construir_instruccionDosOperandos(MOV(_,_),ensamblarDosOperandos(resto)) 
      case "0000" => construir_instruccionDosOperandos(MUL(_,_),ensamblarDosOperandos(resto)) 
      case "0010" => construir_instruccionDosOperandos(ADD(_,_),ensamblarDosOperandos(resto)) 
      case "0011" => construir_instruccionDosOperandos(SUB(_,_),ensamblarDosOperandos(resto)) 
      case "0111" => construir_instruccionDosOperandos(DIV(_,_),ensamblarDosOperandos(resto))
      case "0100" => construir_instruccionDosOperandos(AND(_,_),ensamblarDosOperandos(resto))
      case "0101" => construir_instruccionDosOperandos(OR(_,_),ensamblarDosOperandos(resto))
      case "0110" => construir_instruccionDosOperandos(DIV(_,_),ensamblarDosOperandos(resto))
      case _ => throw new InvalidCodeException("No hay ninguna instruccion con ese codigo de operacion")
    }

  }
  
   def ensamblarPosibleRegistro(cadena_binaria : String): ModoDireccionamiento = {
   val bits =  cadena_binaria.substring(0, 3)  
   val resto = cadena_binaria.takeRight(3)
    bits match {
      case "100" => ensamblarRegistro(resto) 
      case "110" =>  RegistroIndirecto(ensamblarRegistro(resto))  
      case _ => throw new InvalidCodeException("No hay modos de direccionamiento con ese codigo")
    }

  }
  
  
  def ensamblarRegistro(cadena_binaria : String): Registro = {
   
    cadena_binaria match {
      case "000" => R0 
      case "001" => R1 
      case "010" => R2 
      case "011" => R3 
      case "100" => R4 
      case "101" => R5 
      case "110" => R6 
      case "111" => R7 
      case _ => throw new InvalidCodeException("No hay modos de direccionamiento con ese codigo")
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

  println("R7, 0001      " + Ensamblador.ensamblarDosOperandos(d))
  println("R7, R3        " + Ensamblador.ensamblarDosOperandos(b))
  println("R7, [000F]    " + Ensamblador.ensamblarDosOperandos(a))
  println("[000F], R7    " + Ensamblador.ensamblarDosOperandos(c))
  println("[11E1],[FFFF] " + Ensamblador.ensamblarDosOperandos(co))
  
  println("MUL => " + Ensamblador.ensamblarInstruccion("0000" + d))
  println("ADD => " + Ensamblador.ensamblarInstruccion("0010" + b))
  println("ADD SOLO REGISTROS=> " + Ensamblador.ensamblarInstruccion("0010" + registro))
  println("ADD SOLO REGISTROS INDIRECTOS=> " + Ensamblador.ensamblarInstruccion("0010" + inregistro))
  println("ADD directo registro=> " + Ensamblador.ensamblarInstruccion("0010" + directo_registro))
  println("DIV => " + Ensamblador.ensamblarInstruccion("0111" + a))
  println("MOV => " + Ensamblador.ensamblarInstruccion("0001" + c))
  println("SUB => " + Ensamblador.ensamblarInstruccion("0011" + co))
  println("CALL => " + Ensamblador.ensamblarInstruccion("1011" + "000000" + d))
  println("JMP => " + Ensamblador.ensamblarInstruccion("1010" + "000000" + d))
  println("JMP REGISTRO => " + Ensamblador.ensamblarInstruccion("1010" + "000000" + "100111"))
  println("JMP DIRECTO => " + Ensamblador.ensamblarInstruccion("1010" + "000000" + directo))
  println("JE => " + Ensamblador.ensamblarInstruccion("1111" + "0001" + "00000000" ))
  println("JG => " + Ensamblador.ensamblarInstruccion("1111" + "1010" + "00000000" + "1111000011110000" +  "1111000011110000"))
  println("JLE => " + Ensamblador.ensamblarInstruccion("1111" + "0010" + "00000000" +  "1111000011110000"))
  println("JL => " + Ensamblador.ensamblarInstruccion("1111" + "0011" + "00000000" + "1111000011110000" +  "1111000011110000"))
  println("JGE => " + Ensamblador.ensamblarInstruccion("1111" + "1011" + "00000000" + "1111000011110000" +  "1111000011110000"))
  println("JLEU => " + Ensamblador.ensamblarInstruccion("1111" + "0100" + "00000000" + "1111000011110000" +  "1111000011110000"))
  println("JGU => " + Ensamblador.ensamblarInstruccion("1111" + "1100" + "00000000" + "1111000011110000" +  "1111000011110000"))
  println("JCS => " + Ensamblador.ensamblarInstruccion("1111" + "0101" + "00000000" + "1111000011110000" +  "1111000011110000"))
  println("JNEG => " + Ensamblador.ensamblarInstruccion("1111" + "0110" + "10000000" + "1111000011110000" +  "1111000011110000"))
  println("JVS => " + Ensamblador.ensamblarInstruccion("1111" + "0111" + "00000011" + "1111000011110000" +  "1111000011110000"))
  println("JNE => " + Ensamblador.ensamblarInstruccion("1111" + "1001" + "00110000" + "1111000011110000" +  "1111000011110000"))
 

  println("RET => " + Ensamblador.ensamblarInstruccion("1100" + d))
 
  
  // println("PORQUERIA => " + Ensamblador.ensamblarInstruccion("1111" + p))
  //println("PORQUERIA DOS => " + Ensamblador.ensamblarInstruccion("0001" + p))

  
}