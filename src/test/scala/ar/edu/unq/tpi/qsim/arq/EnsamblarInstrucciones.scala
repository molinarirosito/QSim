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
    val call=CALL(R4)
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
  

  def resultados_esperados = new {
   
  }
  

}