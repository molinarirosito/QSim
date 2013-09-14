package ar.edu.unq.tpi.qsim.model
import ar.edu.unq.tpi.qsim.utils.Util

object Ensamblador {
  
  def ensamblar_call() (cadena_binaria : String): Instruccion = {
    val bits : String =  cadena_binaria.substring(0, 6)
    var resultado: Instruccion = null
    bits match {
      case "000000" => resultado //call
      case _ =>  null //ES ERROR PORQUE NO TIENE RELLENO
    }
    resultado
  }
  
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
     bits_segundoOperando = cadena_binaria.substring(6, 13) + cadena_binaria.takeRight(16)
   }
   segundo_operando = ensamblar_operando(bits_segundoOperando)
    Map(("op1",primer_operando),("op2",segundo_operando))
    }
    
  
  
  def ensamblar_operando(cadena_binaria : String): ModoDireccionamiento = {
    println(cadena_binaria)
   val code_modo =  cadena_binaria.substring(0, 6)
   var resto =  cadena_binaria.takeRight(cadena_binaria.size - 7)
   if(resto.size>16) {resto = Util.binary16ToHex(resto.substring(0, 16))}
   else {resto = Util.binary16ToHex(resto.takeRight(16))}

   var modo: ModoDireccionamiento = null
    code_modo match {
      case "000000" => Inmediato(new W16(resto)) 
      case "001000" => Directo(Inmediato(new W16(resto)))
      case _ => ensamblarRegistro(code_modo.takeRight(3))
    }

  }
  
  
  def ensamblar_MOV() (cadena_binaria : String): Instruccion = {
    val bits : String =  cadena_binaria.substring(0, 6)
    var resultado: Instruccion = null
    bits match {
      case "000000" => resultado //call
      case _ =>  null //ES ERROR PORQUE NO TIENE RELLENO
    }
    resultado
  }
  
  def ensamblarInstruccion(cadena_binaria : String): Instruccion = {
    val bits : String =  cadena_binaria.substring(0, 4)
    var resultado: Instruccion = null
    bits match {
      case "1011" => resultado //call
      case "1100" => resultado = RET()
      case "0001" => resultado //mov
      case "0000" => resultado //mul
      case "0010" => resultado //add
      case "0011" => resultado //sub
      case "0111" => resultado //div
      case _ => null
    }
    resultado
  }
  
  def ensamblarModoDireccionamiento(cadena_binaria : String): ModoDireccionamiento = {
   val bits : String =  cadena_binaria.substring(0, 6)
   var modo: ModoDireccionamiento = null
    bits match {
      case "000000" => modo //inmediato
      case "001000" => modo //directo
      case _ => ensamblarRegistro(bits.takeRight(3))
    }
    modo
  }
  def ensamblarRegistro(cadena_binaria : String): ModoDireccionamiento = {
   val bits : String =  cadena_binaria.substring(0, 3)
   
    bits match {
      case "000" => R0 
      case "001" => R1 
      case "010" => R2 
      case "011" => R3 
      case "100" => R4 
      case "101" => R5 
      case "110" => R6 
      case "111" => R7 
      case _ => null
    }

  }
  

}

object pruebados extends App() {

 
  val d = "1001110000000000000000000001" + "11110000111100001111" //R7, 0001
  val b = "100111100011" + "11110000111100001111" + "11110000111100001111"//R7, R3  
  val a = "1001110010000000000000001111" + "11110000111100001111"//R7, [000F]
  val c = "0010001001110001000111100010" + "11110000111100001111" // [000F],R7 relleno
  val co= "001000" + "001000" + "0001000111100001" + "1111111111111111"

  println(Ensamblador.ensamblarDosOperandos(d))
  println(Ensamblador.ensamblarDosOperandos(b))
  println(Ensamblador.ensamblarDosOperandos(a))
  println(Ensamblador.ensamblarDosOperandos(c))
  println(Ensamblador.ensamblarDosOperandos(co))
  
}