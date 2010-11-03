/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.nsc
package ast.parser

import scala.reflect.generic.{ ModifierFlags => Flags }
import transform.Transform

/**
 * A phase running directly after the parser doing simple
 * syntax desugarings:
 *   - lazy function parameters:
 *     `def f(lazy x: Int) = rhs` is rewritten into
 *     `def f(x$lazy: => Int) = {lazy val x$lazy = rhs}`
 */
abstract class Desugar extends Transform { self =>
  import global._

  object treeBuilder extends TreeBuilder {
    val global: self.global.type = self.global
    def freshName(prefix: String): Name = throw new UnsupportedOperationException
    def o2p(offset: Int) = throw new UnsupportedOperationException
    def r2p(start: Int, point: Int, end: Int) = throw new UnsupportedOperationException
  }
  import treeBuilder._

  val phaseName = "desugar"

  def newTransformer(unit: CompilationUnit): Transformer =
    new DesugarTransform(unit)

  class DesugarTransform(unit: CompilationUnit) extends Transformer {
    def isLazy(param: ValDef) =
      param.mods hasFlag Flags.LAZY

    def byNamify(tpt: Tree) =
      AppliedTypeTree(rootScalaDot(nme.BYNAME_PARAM_CLASS_NAME.toTypeName), List(tpt))

    def rewrite(old: ValDef): (ValDef, ValDef) = {
      val newName = (old.name+"$lazy").toTermName
      (old,
       old.copy(
         name = newName,
         mods = old.mods &~ Flags.LAZY | Flags.BYNAMEPARAM,
         tpt = byNamify(old.tpt)))
    }

    override def transform(tree: Tree): Tree = tree match {
      case df@DefDef(_, _, _, vparamss, _, rhs) =>
        val rewrites = Map(vparamss.flatten filter (isLazy) map rewrite:_*)

        if (!rewrites.isEmpty) {
          def rewriteIfLazy(v: ValDef) = rewrites.get(v).getOrElse(v)

          val newParams =
            vparamss.map(_.map(rewriteIfLazy))

          val lazyDecls =
            rewrites map { case (old, newV) => old.copy(rhs = Ident(newV.name)) } toList

          df.copy(vparamss = newParams, rhs = makeBlock(lazyDecls :+ rhs))
        }
        else
          tree
      case _ => super.transform(tree)
    }
  }
}

