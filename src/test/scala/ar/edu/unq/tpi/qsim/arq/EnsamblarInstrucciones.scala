package ar.edu.unq.tpi.qsim.arq

/**
* Copyright 2014 Tatiana Molinari.
* Copyright 2014 Susana Rosito
*
* This program is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with this program. If not, see <http://www.gnu.org/licenses/>.
*
*/


import org.scalatest.FlatSpec
import org.scalatest.Matchers
import ar.edu.unq.tpi.qsim.model._
import scala.collection.mutable._
import ar.edu.unq.tpi.qsim.utils._

class EnsamblarInstrucciones extends FlatSpec with Matchers {

  def obtenerCodigoBinario(instruccion : Instruccion) : String = {
    
    val ins = instruccion.representacionHexadecimal
    var split = ins.split(" ")
    var string_split = ""
    split.foreach(f => {
      string_split = string_split + Util.hexToBinary(f)
     })
    string_split
  }

  def obtenerOperandosBinario(instruccion : Instruccion) :String = {
   val instruccion_binario = obtenerCodigoBinario(instruccion) 
   instruccion_binario.takeRight(instruccion_binario.size - 4) 
    
  }
  def contexto_ejecucion = new {
    
    //modos de direccionamiento
    val registro = R0
    val registroIndirecto = RegistroIndirecto(R7)
    val inmediato = Inmediato("F0F0")
    val directo = Directo(Inmediato("ABCD"))
    val indirecto = Indirecto(Directo(Inmediato("F0CA")))
   
    //instrucciones sin operandos
    val ret = RET()
    
    //instruccione modo origen
    
    val call_registro = CALL(registro)
    val call_registroIndirecto = CALL(registroIndirecto)

    val call= call_registro

    val jmp_inmediato = JMP(inmediato)
    val jmp_directo = JMP(directo)
    val jmp_indirecto = JMP(indirecto)    
    
    val jmp=jmp_inmediato
    
    //instruccion modo destino
    
    val not_registro = NOT(registro)
    val not_registroIndirecto = NOT(registroIndirecto)
    val not_directo = NOT(directo)
    val not_indirecto = NOT(indirecto)
    
    val not= not_registro
    
    
    //instrucciones dos operandos

      
    val registro_registro = MUL(registro, registro)
    val registro_registroIndirecto = MUL(registro, registroIndirecto)
    val registro_inmediato = MUL(registro, inmediato)
    val registro_directo = MUL(registro, directo)
    val registro_indirecto = MUL(registro, indirecto)
    
    val registroIndirecto_registro = MUL(registroIndirecto, registro)
    val registroIndirecto_registroIndirecto = MUL(registroIndirecto, registroIndirecto)
    val registroIndirecto_inmediato = MUL(registroIndirecto, inmediato)
    val registroIndirecto_directo = MUL(registroIndirecto, directo)
    val registroIndirecto_indirecto = MUL(registroIndirecto, indirecto)
     
    val directo_registro = MUL(directo, registro)
    val directo_registroIndirecto = MUL(directo, registroIndirecto)
    val directo_inmediato = MUL(directo, inmediato)
    val directo_directo = CMP(directo, directo)
    val directo_indirecto = OR(directo, indirecto)
    
    val indirecto_registro = ADD(indirecto, registro)
    val indirecto_registroIndirecto = DIV(indirecto, registroIndirecto)
    val indirecto_inmediato = SUB(indirecto, inmediato)
    val indirecto_directo = MOV(indirecto, directo)
    val indirecto_indirecto = AND(indirecto, indirecto)
    
    val mul = directo_inmediato
    val add = indirecto_registro
    val div = indirecto_registroIndirecto
    val sub = indirecto_inmediato
    val mov = indirecto_directo
    val and = indirecto_indirecto
    val or = directo_indirecto
    val cmp = directo_directo
    
    // jmps condicionales
    
    
    val je =  JE(new Salto(255)) //11111111
	val jne = JNE(new Salto(1))  //00000001
	val jle = JLE(new Salto(2))  //00000001
	val jg = JG(new Salto(100))    //01100100
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
    
   def resultados_codigos_modos_dir = new {
    val registro = "100000"
    val registro_indirecto = "110111"
    val inmediato = "000000"
    val valor_inmediato = "1111000011110000" //F0F0
    val directo = "001000" 
    val valor_directo = "1010101111001101" //ABCD
    val indirecto = "011000" 
    val valor_indirecto = "1111000011001010" //F0CA
  }
   
   "Los ensamblamientos de instrucciones con jmps condicionales" should ("ensamblarse y devolver en hexadecimal el equivalente en binario al formato el numero" 
 + "corresponda segun cuantas celdas deban saltarse luego del codigo de operacion y el relleno") in {
    var ctx = contexto_ejecucion
    var resultado = resultados_codigos_modos_dir

    assert(obtenerCodigoBinario(ctx.je).endsWith("11111111"))
    assert(obtenerCodigoBinario(ctx.jne).endsWith("00000001"))
    assert(obtenerCodigoBinario(ctx.jle).endsWith("00000010"))
    assert(obtenerCodigoBinario(ctx.jg).endsWith("01100100"))
    
  }
   
   "Los ensamblamientos de instrucciones con un operando destino" should ("ensamblarse y devolver en hexadecimal el equivalente en binario al formato modo destino" 
 + "(6 bits), destino (16 bits) luego de los primer 4 bits de modo de operacion y antes de los seis bits en cero de relleno ") in {
    var ctx = contexto_ejecucion
    var resultado = resultados_codigos_modos_dir

    assert(obtenerOperandosBinario(ctx.not_registro).startsWith(resultado.registro))
    assert(obtenerOperandosBinario(ctx.not_registroIndirecto).startsWith(resultado.registro_indirecto))
    assert(obtenerOperandosBinario(ctx.not_directo).startsWith(resultado.directo + "000000" + resultado.valor_directo))
    assert(obtenerOperandosBinario(ctx.not_indirecto).startsWith(resultado.indirecto  + "000000" + resultado.valor_indirecto))
    
  }
   
    "Los ensamblamientos de instrucciones con un operando origen" should ("ensamblarse y devolver en hexadecimal el equivalente en binario al formato modo destino" 
 + "(6 bits), destino (16 bits) luego de los primer 4 bits de modo de operacion y de los seis bits en cero de relleno ") in {
    var ctx = contexto_ejecucion
    var resultado = resultados_codigos_modos_dir
          
    
    assert(obtenerOperandosBinario(ctx.call_registro).endsWith(resultado.registro))
    assert(obtenerOperandosBinario(ctx.call_registroIndirecto).endsWith(resultado.registro_indirecto))
    assert(obtenerOperandosBinario(ctx.jmp_inmediato).endsWith(resultado.inmediato + resultado.valor_inmediato))
    assert(obtenerOperandosBinario(ctx.jmp_directo).endsWith(resultado.directo + resultado.valor_directo))
    assert(obtenerOperandosBinario(ctx.jmp_indirecto).endsWith(resultado.indirecto  + resultado.valor_indirecto))
 
    
  }
  
  "Los ensamblamientos de instrucciones con dos operandos" should ("ensamblarse y devolver en hexadecimal el equivalente en binario al formato modo destino" 
 + "(6 bits), modo origen (6 bits), destino (16 bits), orgen (16 bits) luego de los primer 4 bits de modo de operacion  ") in {
    var ctx = contexto_ejecucion
    var resultado = resultados_codigos_modos_dir
    
    assert(obtenerOperandosBinario(ctx.registro_registro).endsWith(resultado.registro + resultado.registro))
    assert(obtenerOperandosBinario(ctx.registro_registroIndirecto).endsWith(resultado.registro + resultado.registro_indirecto))
    assert(obtenerOperandosBinario(ctx.registro_inmediato).endsWith(resultado.registro + resultado.inmediato + resultado.valor_inmediato ))
    assert(obtenerOperandosBinario(ctx.registro_directo).endsWith(resultado.registro + resultado.directo + resultado.valor_directo ))
    assert(obtenerOperandosBinario(ctx.registro_indirecto).endsWith(resultado.registro + resultado.indirecto + resultado.valor_indirecto ))
        
    assert(obtenerOperandosBinario(ctx.registroIndirecto_registro).endsWith(resultado.registro_indirecto + resultado.registro))
    assert(obtenerOperandosBinario(ctx.registroIndirecto_registroIndirecto).endsWith(resultado.registro_indirecto + resultado.registro_indirecto))
    assert(obtenerOperandosBinario(ctx.registroIndirecto_inmediato).endsWith(resultado.registro_indirecto + resultado.inmediato + resultado.valor_inmediato ))
    assert(obtenerOperandosBinario(ctx.registroIndirecto_directo).endsWith(resultado.registro_indirecto + resultado.directo + resultado.valor_directo ))
    assert(obtenerOperandosBinario(ctx.registroIndirecto_indirecto).endsWith(resultado.registro_indirecto + resultado.indirecto + resultado.valor_indirecto ))
    
    assert(obtenerOperandosBinario(ctx.directo_registro).endsWith(resultado.directo + resultado.registro + resultado.valor_directo))
    assert(obtenerOperandosBinario(ctx.directo_registroIndirecto).endsWith(resultado.directo + resultado.registro_indirecto + resultado.valor_directo))
    assert(obtenerOperandosBinario(ctx.directo_inmediato).endsWith(resultado.directo +  resultado.inmediato + resultado.valor_directo + resultado.valor_inmediato ))
    assert(obtenerOperandosBinario(ctx.directo_directo).endsWith(resultado.directo  + resultado.directo + resultado.valor_directo + resultado.valor_directo ))
    assert(obtenerOperandosBinario(ctx.directo_indirecto).endsWith(resultado.directo + resultado.indirecto + resultado.valor_directo + resultado.valor_indirecto ))
    
    assert(obtenerOperandosBinario(ctx.indirecto_registro).endsWith(resultado.indirecto + resultado.registro + resultado.valor_indirecto))
    assert(obtenerOperandosBinario(ctx.indirecto_registroIndirecto).endsWith(resultado.indirecto + resultado.registro_indirecto + resultado.valor_indirecto))
    assert(obtenerOperandosBinario(ctx.indirecto_inmediato).endsWith(resultado.indirecto +  resultado.inmediato + resultado.valor_indirecto + resultado.valor_inmediato ))
    assert(obtenerOperandosBinario(ctx.indirecto_directo).endsWith(resultado.indirecto  + resultado.directo + resultado.valor_indirecto + resultado.valor_directo ))
    assert(obtenerOperandosBinario(ctx.indirecto_indirecto).endsWith(resultado.indirecto + resultado.indirecto + resultado.valor_indirecto + resultado.valor_indirecto ))
  }
  
  
 "Las instrucciones sin operandos" should "ensamblarse y devolver en hexadecimal el equivalente en binario a su codigo de opercion correspondiente mas 12 bits en cero" in {
    var ctx = contexto_ejecucion
    var resultados = resultados_codigos_instrucciones_sin_operandos 
    
    assert(Util.hexTo4Binary(ctx.ret.representacionHexadecimal.head.toString).equals(resultados.ret))
    assert(obtenerCodigoBinario(ctx.ret).equals(resultados.ret + "000000000000"))
   
  }
  
  "Las instrucciones con un operando origen" should "ensamblarse y devolver en hexadecimal el equivalente en los primeros 10 bits binarios a su codigo de opercion correspondiente mas 6 bits en cero" in {
    var ctx = contexto_ejecucion
    var resultados = resultados_codigos_un_modo_origen 
    
    assert(Util.hexTo4Binary(ctx.call.representacionHexadecimal.head.toString).equals(resultados.call))
    assert(obtenerCodigoBinario(ctx.call).substring(0, 10).equals(resultados.call + "000000"))
    
    assert(Util.hexTo4Binary(ctx.jmp.representacionHexadecimal.head.toString).equals(resultados.jmp))
    assert(obtenerCodigoBinario(ctx.jmp).substring(0, 10).equals(resultados.jmp + "000000"))
   
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