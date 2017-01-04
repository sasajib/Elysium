package nz.daved.elysium.misc

import nz.daved.elysium.gen.macroAnnotation

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.meta._

object Identities {
  @macroAnnotation
  def identity[A <: Tree](tree: A): A = tree
}

@compileTimeOnly("@copyDef not expanded")
class copyDef extends StaticAnnotation {
  inline def apply(a: Any): Any = meta {
    a match {
      case defn: Defn.Def =>
        defn.copy()
      case _ =>
        abort("@copyDef only supports Defn")
    }
  }
}

