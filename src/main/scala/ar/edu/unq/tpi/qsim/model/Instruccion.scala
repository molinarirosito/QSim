package ar.edu.unq.tpi.qsim.model
import ar.edu.unq.tpi.qsim.utils._

abstract class Instruccion(val codigoDeOperacion: String, var operacion: String)
{

  def representacionHexadecimal() : String
  def decode() : String = this.toString 
  def tamanioHex() : Int = this.representacionHexadecimal.replace(" ", "").size
  def cantidadCeldas() : Int = this.tamanioHex/4
  
}

trait InstruccionConEtiqueta {
  
  def etiqueta(): String  
}

class Instruccion_SinOperandos(codigoDeOperacion: String, operacion: String, val relleno: String) extends Instruccion(codigoDeOperacion, operacion) with InstruccionConEtiqueta {
  
  override def etiqueta() = "" 
    
  override def representacionHexadecimal() : String  =  {
 
  val operations_code_binary = Util.binary16ToHex(codigoDeOperacion + relleno )
   
  (operations_code_binary).replace("  "," ")
  }
  override def toString() =  operacion 
}

class Instruccion_UnOperando(codigoDeOperacion: String, operacion: String, var origen: ModoDireccionamiento, val relleno: String) extends Instruccion(codigoDeOperacion, operacion) with InstruccionConEtiqueta  {
  
  override def etiqueta() = ""
  
  override def representacionHexadecimal() : String  =  {
 
  val operations_code_binary = Util.binary16ToHex(codigoDeOperacion + relleno + origen.codigo )
   
  val origen_value = origen.getValorString
    
  (operations_code_binary + " " + origen_value).replace("  "," ")
  }
  
  override def toString() =  operacion + " " + origen.toString()
}

class Instruccion_DosOperandos(codigoDeOperacion: String, operacion: String, var destino: ModoDireccionamiento, var origen: ModoDireccionamiento) extends Instruccion(codigoDeOperacion, operacion)
{
	override def representacionHexadecimal() : String  =  {
			
		val operations_code_binary = Util.binary16ToHex(codigoDeOperacion + destino.codigo + origen.codigo)
				
		val destiny_value = destino.getValorString
		val origin_value = origen.getValorString
					
		(operations_code_binary + " " + destiny_value + " " + origin_value).replace("  "," ")
	}
	
	override def toString() =  operacion + " " + destino.toString() + " " + origen.toString() 
}

class JUMP_condicional(codigoDeOperacion: String, operacion: String, var desplazamiento: W8) extends Instruccion(codigoDeOperacion, operacion){
  val prefijo = "1111"
  var destino : ModoDireccionamiento = _ 
  override def representacionHexadecimal() : String  =  {
 
  val operations_code_binary = Util.binary16ToHex(prefijo + codigoDeOperacion + desplazamiento.toBinary )
   
  (operations_code_binary).replace("  "," ")
  }
  override def toString() =  operacion + desplazamiento.toString
}

case class JE(desp: W8) extends JUMP_condicional("0001","JE", desp){}
case class JNE(desp: W8) extends JUMP_condicional("1001","JNE", desp){}
case class JLE(desp: W8) extends JUMP_condicional("0010","JLE", desp){}
case class JG(desp: W8) extends JUMP_condicional("1010","JG", desp){}
case class JL(desp: W8) extends JUMP_condicional("0011","JL", desp){}
case class JGE(desp: W8) extends JUMP_condicional("1011","JGE", desp){}
case class JLEU(desp: W8) extends JUMP_condicional("0100","JLEU", desp){}
case class JGU(desp: W8) extends JUMP_condicional("1100","JGU", desp){}
case class JCS(desp: W8) extends JUMP_condicional("0101","JCS", desp){}
case class JNEG(desp: W8) extends JUMP_condicional("0110","JNEG", desp){}
case class JVS(desp: W8) extends JUMP_condicional("0111","JVS", desp){}


case class CALL(orig: ModoDireccionamiento) extends Instruccion_UnOperando("1011","CALL",orig, "000000"){}

case class RET() extends Instruccion_SinOperandos("1100","RET", "000000000000"){}

case class JMP(orig: ModoDireccionamiento) extends Instruccion_UnOperando("1010","JMP",orig, "000000"){}

case class MUL(dest: ModoDireccionamiento, orig: ModoDireccionamiento) extends Instruccion_DosOperandos("0000","MUL",dest,orig){}

case class ADD(dest: ModoDireccionamiento, orig: ModoDireccionamiento) extends Instruccion_DosOperandos("0010","ADD",dest,orig){}

case class SUB(dest: ModoDireccionamiento, orig: ModoDireccionamiento) extends Instruccion_DosOperandos("0011","SUB",dest,orig){}

case class DIV(dest: ModoDireccionamiento, orig: ModoDireccionamiento) extends Instruccion_DosOperandos("0111","DIV",dest,orig){}

case class MOV(dest: ModoDireccionamiento, orig: ModoDireccionamiento) extends Instruccion_DosOperandos("0001","MOV",dest,orig){}


object Testuds extends App{


  val e = ADD(R1,R5)
  println(e.toString())
  println(e.representacionHexadecimal())
  println(e.tamanioHex)
}
