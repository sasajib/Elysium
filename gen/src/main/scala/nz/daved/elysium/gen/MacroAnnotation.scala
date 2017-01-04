package nz.daved.elysium.gen

import scala.annotation.StaticAnnotation
import scala.meta._
import scala.collection.immutable._
import scala.meta.dialects.Paradise211

/**
  * The eventual idea here is to lift plain functions/methods into the context of a macro
  *
  * This does:
  *
  * - Check the input tree type.
  * - Check the return tree type
  * - Check all parameter types.
  * - Converts parameter literals to the literal type they are
  * - Makes parameter literals available in scope as the named parameter
  *
  * // TODO actually just call the other method, rather then manually inlining it
  * eg.
  *
  * @macroAnnotation
  * def identity(tree: meta.Tree): meta.Tree = tree
  *
  * becomes:
  *
  *
  * class identity extends StaticAnnotation {
  *   inline def apply(a: Any): Any = meta {
  *      a match {
  *        case tree: meta.Tree =>
  *         tree match {
  *            case t: meta.Tree =>
  *              t
  *            case _ =>
  *              abort("@identity must return a meta.Tree")
  *         }
  *        case _ =>
  *         abort("@identity only supports meta.Tree")
  *      }
  *   }
  * }
  *
  * and
  *
  * @macroAnnotation
  * def foo[T](bar: Defn.Val)(baz: String): Defn.Val = ???
  *
  * becomes:
  *
  * class foo[T](_baz: String) extends StaticAnnotation {
  *   inline def apply(a: Any): Any = meta {
  *      val baz = s match {
  *        case Lit(s: String) =>
  *          s
  *        case _ =>
  *          abort(expected 'baz' to be a string literal)
  *      }
  *
  *      a match {
  *        case bar: Defn.Val =>
  *         ???
  *        case _ =>
  *         abort("'Foo' only supports Defn.Val")
  *      }
  *   }
  * }
  */
class macroAnnotation extends StaticAnnotation {
  inline def apply(a: Any): Any = meta {
    val defn: Defn.Def = MacroAnnotation.asDef(a)

    val stats: Seq[Stat] = defn.body match {
      case block: Term.Block =>
        block.stats
      case other =>
        other :: Nil
    }

    val inputTreeParam = defn.paramss.head
    val treeName = inputTreeParam.head.name
    val newStats: Seq[Stat] = q"inline def apply($treeName: Any): Any = meta { ..$stats }" :: Nil
    val className: Type.Name = Type.Name(defn.name.value)
    val newClass: Defn.Class = q"""class $className[..${defn.tparams}] extends scala.annotation.StaticAnnotation { ..$newStats }"""
    newClass
  }
}

object MacroAnnotation {
  // TODO: Tighten restrictions here
  // - Check for
  def asDef(a: Any): Defn.Def = {
    a match {
      case defn: Defn.Def =>
        defn
      case _ =>
        abort("Currently macroAnnotation only supports def's")
    }
  }
}