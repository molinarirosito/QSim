package ar.edu.unq.tpi.qsim.integracion.mumuki

import com.google.gson.Gson
import com.google.gson.GsonBuilder
import ar.edu.unq.tpi.qsim.model.CPU
import ar.edu.unq.tpi.qsim.model.Registro

case class JsonOk(var special_records: SpecialRecords, var flags: Flags, var records: Records)
case class SpecialRecords(val PC: String, val SP: String, val IR: String)
case class Flags(val N: Int, val Z: Int, val V: Int, val C: Int)
case class Records(val R0: String, val R1: String, val R2: String, val R3: String, val R4: String,
                   val R5: String, val R6: String, val R7: String)
case class JsonError(val error: String)

class JsonResult {

  implicit def registroToString(registro: Registro): String = registro.valor.hex

  var gson = new GsonBuilder().setPrettyPrinting().create();
  var result: Any = _

  def buildJsonOk(cpu: CPU): Any = {
    result = JsonOk(
      SpecialRecords(cpu.pc.hex, cpu.sp.hex, cpu.ir),
      Flags(cpu.n, cpu.z, cpu.v, cpu.c),
      Records(cpu.registros(0), cpu.registros(1), cpu.registros(2), cpu.registros(3), cpu.registros(4),
        cpu.registros(5), cpu.registros(6), cpu.registros(7)))
    printJson()
  }

  def buildJsonError(msg: String): Any = {
    result = JsonError(msg)
    printJson()
  }

  def printJson() {
    Console.println(gson.toJson(result))
  }
}