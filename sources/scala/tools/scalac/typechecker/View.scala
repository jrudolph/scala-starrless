/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**
** $Id$
\*                                                                      */
import scalac.{symtab => scalac_symtab}
import scalac.ast._;

package scala.tools.scalac.typechecker {

import scalac_symtab._;

case class View(sym: Symbol, symtype: Type, qual: Tree, context: Context) {
  var locked = false;
}

}
