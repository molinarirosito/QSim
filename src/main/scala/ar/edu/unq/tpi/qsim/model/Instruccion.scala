package ar.edu.unq.tpi.qsim.model
import ar.edu.unq.tpi.qsim.utils._

abstract class Instruccion(val codigoDeOperacion: String, var operacion: String)
{

  def representacionHexadecimal() : String
  def decode() : String = this.toString 
  def tamanioHex() : Int = this.representacionHexadecimal.replace(" ", "").size
  def cantidadCeldas() : Int = this.tamanioHex/4
  
}

class Instruccion_UnOperando(codigoDeOperacion: String, operacion: String, var origen: ModoDireccionamiento, val relleno: String) extends Instruccion(codigoDeOperacion, operacion) {
  
  override def representacionHexadecimal() : String  =  {
 
  val operations_code_binary = Util.binary16ToHex(codigoDeOperacion + relleno + origen.codigo )
   
  val origen_value = origen.getValorString
    
  (operations_code_binary + " " + origen_value).replace("  "," ")
}
}

case class CALL(orig: ModoDireccionamiento) extends Instruccion_UnOperando("1011","CALL",orig, "000000"){

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

case class MUL(dest: ModoDireccionamiento, orig: ModoDireccionamiento) extends Instruccion_DosOperandos("0000","MUL",dest,orig){

}

case class ADD(dest: ModoDireccionamiento, orig: ModoDireccionamiento) extends Instruccion_DosOperandos("0010","ADD",dest,orig){
}

case class SUB(dest: ModoDireccionamiento, orig: ModoDireccionamiento) extends Instruccion_DosOperandos("0011","SUB",dest,orig){
}

case class DIV(dest: ModoDireccionamiento, orig: ModoDireccionamiento) extends Instruccion_DosOperandos("0111","DIV",dest,orig){
}

case class MOV(dest: ModoDireccionamiento, orig: ModoDireccionamiento) extends Instruccion_DosOperandos("0001","MOV",dest,orig){
}


object Testuds extends App{


  val e = ADD(R1,R5)
  println(e.toString())
  println(e.representacionHexadecimal())
  println(e.tamanioHex)
}
