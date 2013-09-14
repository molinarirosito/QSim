package ar.edu.unq.tpi.qsim.model
import ar.edu.unq.tpi.qsim.utils.Util
import ar.edu.unq.tpi.qsim.exeptions._

object Ensamblador {
  
  
  def ensamblarDosOperandos(cadena_binaria : String) : Map[String,ModoDireccionamiento] = {
   
   var bits_segundoOperando : String = ""
   var primer_operando : ModoDireccionamiento = null
   var segundo_operando : ModoDireccionamiento = null
   
   if(cadena_binaria.startsWith("100"))
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
    
  
  
  def ensamblar_operando(cadena_binaria : String): ModoDireccionamiento = {

   val code_modo =  cadena_binaria.substring(0, 6)
   var resto =  cadena_binaria.takeRight(cadena_binaria.size - 6) 
   if(resto.size>16) {resto = Util.binary16ToHex(resto.substring(0, 16))}
   else {resto = Util.binary16ToHex(resto.takeRight(16))}

   var modo: ModoDireccionamiento = null
    code_modo match {
      case "000000" => Inmediato(new W16(resto)) 
      case "001000" => Directo(Inmediato(new W16(resto)))
      case _ => ensamblarRegistro(code_modo)
    }

  }
  
  
  def ensamblar_CALL (cadena_binaria : String): Instruccion = {
    val relleno =  cadena_binaria.substring(0, 6)
    val resto =  cadena_binaria.takeRight(cadena_binaria.size - 6)
    relleno match {
      case "000000" => CALL(ensamblar_operando(resto))
      case _ =>  throw new InvalidCodeException("Podría haber sido un CALL pero no posee relleno") //ES ERROR PORQUE NO TIENE RELLENO
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
      case "1011" => ensamblar_CALL(resto)
      case "1100" => RET()
      case "0001" => construir_instruccionDosOperandos(MOV(_,_),ensamblarDosOperandos(resto)) //mov
      case "0000" => construir_instruccionDosOperandos(MUL(_,_),ensamblarDosOperandos(resto)) //mul
      case "0010" => construir_instruccionDosOperandos(ADD(_,_),ensamblarDosOperandos(resto)) //add
      case "0011" => construir_instruccionDosOperandos(SUB(_,_),ensamblarDosOperandos(resto)) //sub
      case "0111" => construir_instruccionDosOperandos(DIV(_,_),ensamblarDosOperandos(resto)) //div
      case _ => throw new InvalidCodeException("No hay ninguna instruccion con ese codigo de operacion")
    }

  }
  
  
  def ensamblarRegistro(cadena_binaria : String): ModoDireccionamiento = {
   
    cadena_binaria match {
      case "100000" => R0 
      case "100001" => R1 
      case "100010" => R2 
      case "100011" => R3 
      case "100100" => R4 
      case "100101" => R5 
      case "100110" => R6 
      case "100111" => R7 
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

  println("R7, 0001      " + Ensamblador.ensamblarDosOperandos(d))
  println("R7, R3        " + Ensamblador.ensamblarDosOperandos(b))
  println("R7, [000F]    " + Ensamblador.ensamblarDosOperandos(a))
  println("[000F], R7    " + Ensamblador.ensamblarDosOperandos(c))
  println("[11E1],[FFFF] " + Ensamblador.ensamblarDosOperandos(co))
  
  println("MUL => " + Ensamblador.ensamblarInstruccion("0000" + d))
  println("ADD => " + Ensamblador.ensamblarInstruccion("0010" + b))
  println("DIV => " + Ensamblador.ensamblarInstruccion("0111" + a))
  println("MOV => " + Ensamblador.ensamblarInstruccion("0001" + c))
  println("SUB => " + Ensamblador.ensamblarInstruccion("0011" + co))
  println("CALL => " + Ensamblador.ensamblarInstruccion("1011" + "000000" + d))
  println("RET => " + Ensamblador.ensamblarInstruccion("1100" + d))
  println("PORQUERIA => " + Ensamblador.ensamblarInstruccion("1111" + p))
  println("PORQUERIA DOS => " + Ensamblador.ensamblarInstruccion("0001" + p))

  
}