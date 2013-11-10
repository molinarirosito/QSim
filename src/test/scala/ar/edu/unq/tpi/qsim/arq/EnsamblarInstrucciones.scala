package ar.edu.unq.tpi.qsim.arq

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import ar.edu.unq.tpi.qsim.model._
import scala.collection.mutable._
import ar.edu.unq.tpi.qsim.utils._

class EnsamblarInstrucciones extends FlatSpec with Matchers {

  def contexto_ejecucion = new {
   
    //instrucciones sin operandos
    val ret = RET()
    
    //instruccione modo origen
    val call=CALL(Inmediato("000F"))
    val jmp=JMP(Inmediato("000F"))
    
    //instruccione modo destino
    val not=NOT(R4)
    
    //instrucciones dos operandos
    val mul = MUL(R4, Inmediato("F0F0"))
    val add = ADD( Inmediato("F0F0"), Directo(Inmediato("FFFF")))
    val div = DIV(Directo(Inmediato("0001")), Indirecto(Directo(Inmediato("0002"))))
    val sub = SUB(Indirecto(Directo(Inmediato("0003"))), RegistroIndirecto(R5))
    val mov = MOV(RegistroIndirecto(R5), R4)
    val and = AND(R4, Inmediato("0004"))
    val or = OR(R4, Inmediato("0005"))
    val cmp = CMP(R4, Inmediato("0006"))
    
    // jmps condicionales
    
    val je =  JE(new Salto(0)) 
	val jne = JNE(new Salto(1)) 
	val jle = JLE(new Salto(2)) 
	val jg = JG(new Salto(3)) 
	val jl = JL(new Salto(4)) 
	val jge = JGE(new Salto(5)) 
	val jleu = JLEU(new Salto(6)) 
	val jgu = JGU(new Salto(7)) 
	val jcs = JCS(new Salto(8)) 
	val jneg = JNEG(new Salto(0)) 
	val jvs = JVS(new Salto(0)) 
   
    
  }
  
   def resultados_codigos_instrucciones_sin_operandos = new {
    val ret = "1100"
 
  }
  
   def resultados_codigos_un_modo_origen = new {
    val call = "1011"
    val jmp = "1010"
 
  }
   
   def resultados_codigos_un_modo_destino = new {
    val not = "1001"
  }

  def resultados_codigos_instrucciones_dos_operandos = new {
    val mul = "0000"
    val add = "0010"
    val div = "0111"
    val sub = "0011"
    val mov = "0001"
    val and = "0100"
    val or = "0101"
    val cmp = "0110"  
  }

  def resultados_codigos_instrucciones_jmps_condicionales = new {
    val je =  "0001"
	val jne = "1001" 
	val jle = "0010" 
	val jg = "1010"
	val jl = "0011" 
	val jge = "1011" 
	val jleu = "0100" 
	val jgu = "1100" 
	val jcs = "0101" 
	val jneg = "0110" 
	val jvs = "0111" 
  }
    
  
  "Las instrucciones sin operandos" should "ensamblarse y devolver en hexadecimal el equivalente en binario a su codigo de opercion correspondiente mas 12 bits en cero" in {
    var ctx = contexto_ejecucion
    var resultados = resultados_codigos_instrucciones_sin_operandos 
    
    assert(Util.hexTo4Binary(ctx.ret.representacionHexadecimal.head.toString).equals(resultados.ret))
    assert(Util.hexToBinary(ctx.ret.representacionHexadecimal).equals(resultados.ret + "000000000000"))
   
  }
  
  "Las instrucciones con un operando origen" should "ensamblarse y devolver en hexadecimal el equivalente en los primeros 10 bits binarios a su codigo de opercion correspondiente mas 6 bits en cero" in {
    var ctx = contexto_ejecucion
    var resultados = resultados_codigos_un_modo_origen 
    
    assert(Util.hexTo4Binary(ctx.call.representacionHexadecimal.head.toString).equals(resultados.call))
    assert(Util.hexToBinary(ctx.call.representacionHexadecimal.substring(0, 4)).substring(0, 10).equals(resultados.call + "000000"))
    
    assert(Util.hexTo4Binary(ctx.jmp.representacionHexadecimal.head.toString).equals(resultados.jmp))
    assert(Util.hexToBinary(ctx.jmp.representacionHexadecimal.substring(0, 4)).substring(0, 10).equals(resultados.jmp + "000000"))
   
  }
  
   "Las instrucciones con un operando destino" should "ensamblarse y devolver en hexadecimal el equivalente en los primeros 4 bits binarios a su codigo de opercion correspondiente y los ultimos 6 bits en cero" in {
    var ctx = contexto_ejecucion
    var resultados = resultados_codigos_un_modo_destino 
    
    assert(Util.hexTo4Binary(ctx.not.representacionHexadecimal.head.toString).equals(resultados.not))
    assert(Util.hexToBinary(ctx.not.representacionHexadecimal.substring(0, 4)).takeRight(6) .equals("000000"))

  }
    
    "Las instrucciones de dos operandos" should "ensamblarse y devolver en el primer digito hexadecimal el equivalente en binario a su codigo de instruccion" in {
    var ctx = contexto_ejecucion
    var resultados = resultados_codigos_instrucciones_dos_operandos
    

    assert(Util.hexTo4Binary(ctx.mul.representacionHexadecimal.head.toString).equals(resultados.mul))
    assert(Util.hexTo4Binary(ctx.div.representacionHexadecimal.head.toString).equals(resultados.div))
    assert(Util.hexTo4Binary(ctx.add.representacionHexadecimal.head.toString).equals(resultados.add))
    assert(Util.hexTo4Binary(ctx.sub.representacionHexadecimal.head.toString).equals(resultados.sub))
    assert(Util.hexTo4Binary(ctx.mov.representacionHexadecimal.head.toString).equals(resultados.mov))
    assert(Util.hexTo4Binary(ctx.and.representacionHexadecimal.head.toString).equals(resultados.and))
    assert(Util.hexTo4Binary(ctx.or.representacionHexadecimal.head.toString).equals(resultados.or))
    assert(Util.hexTo4Binary(ctx.cmp.representacionHexadecimal.head.toString).equals(resultados.cmp))
  }
   
    "Las instrucciones jumps condicionales" should "ensamblarse y devolver en hexadecimal el equivalente en los primeros 4 bits binarios al relleno 1111 y los siguientes 4 su codigo de opercion correspondiente" in {
    var ctx = contexto_ejecucion
    var resultados = resultados_codigos_instrucciones_jmps_condicionales 
   
    assert(Util.hexToBinary(ctx.jneg.representacionHexadecimal.substring(0, 4)).substring(0, 8).equals("1111" + resultados.jneg))
    assert(Util.hexToBinary(ctx.jne.representacionHexadecimal.substring(0, 4)).substring(0, 8).equals("1111" + resultados.jne))
    assert(Util.hexToBinary(ctx.je.representacionHexadecimal.substring(0, 4)).substring(0, 8).equals("1111" + resultados.je))
    assert(Util.hexToBinary(ctx.jle.representacionHexadecimal.substring(0, 4)).substring(0, 8).equals("1111" + resultados.jle))
    assert(Util.hexToBinary(ctx.jg.representacionHexadecimal.substring(0, 4)).substring(0, 8).equals("1111" + resultados.jg))
    assert(Util.hexToBinary(ctx.jl.representacionHexadecimal.substring(0, 4)).substring(0, 8).equals("1111" + resultados.jl))
    assert(Util.hexToBinary(ctx.jge.representacionHexadecimal.substring(0, 4)).substring(0, 8).equals("1111" + resultados.jge))
    assert(Util.hexToBinary(ctx.jleu.representacionHexadecimal.substring(0, 4)).substring(0, 8).equals("1111" + resultados.jleu))
    assert(Util.hexToBinary(ctx.jgu.representacionHexadecimal.substring(0, 4)).substring(0, 8).equals("1111" + resultados.jgu))
    assert(Util.hexToBinary(ctx.jcs.representacionHexadecimal.substring(0, 4)).substring(0, 8).equals("1111" + resultados.jcs))
    assert(Util.hexToBinary(ctx.jvs.representacionHexadecimal.substring(0, 4)).substring(0, 8).equals("1111" + resultados.jvs))

  }
  
  

}