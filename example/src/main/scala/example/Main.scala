package example

import codegen.generated._

object Main {

  def main(args: Array[String]): Unit = {
    val x = Pet(1L, "halo", None)
    println(x)
  }
}
