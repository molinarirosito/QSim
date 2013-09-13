package ar.edu.unq.tpi.qsim.model

object Ensamblador {
  
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