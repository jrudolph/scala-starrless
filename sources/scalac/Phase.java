/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac;

import scalac.ast.printer.TreePrinter;
import scalac.symtab.Symbol;
import scalac.symtab.Type;
import scalac.checkers.Checker;

public abstract class Phase {

    //########################################################################
    // Public Fields

    /** The global environment */
    public final Global global;

    /** The phase descriptor */
    public final PhaseDescriptor descriptor;

    /** The phase identifier */
    public final int id;

    /** The previous phase */
    public final Phase prev;

    /** The next phase */
    public Phase next;

    //########################################################################
    // Public Constructors

    /** Initializes this instance. */
    public Phase(Global global, PhaseDescriptor descriptor) {
        this.global = global;
        this.descriptor = descriptor;
        this.id = descriptor.id();
        this.prev = global.currentPhase;
        if (prev != null) prev.next = this;
        descriptor.initialize(this);
        global.currentPhase = this;
    }

    //########################################################################
    // Public Methods

    /** Does this phase precede the given phase? */
    public boolean precedes(Phase phase) {
        return id < phase.id;
    }

    /**
     * Returns the info of `sym' after the phase. Assumes that `tp' is
     * the info of symbol `sym' before this phase.
     */
    public Type transformInfo(Symbol sym, Type tp) {
        return tp;
    }

    /** Applies this phase to the given compilation units. */
    public abstract void apply(CompilationUnit[] units);

    /** Graphs all compilation units. */
    public void graph(Global global) {
        for (int i = 0; i < global.units.length; i++) graph(global.units[i]);
    }

    /** Graphs the result of this phase for the given compilation unit. */
    public void graph(CompilationUnit unit) {
        // !!! new scala.compiler.gdl.TreePrinter().printInFile(
        // !!!     unit, unit.source + "-" + name() + ".gdl");
    }

    /** Checks all compilation units. */
    public void check(Global global) {
        for (int i = 0; i < global.units.length; i++) check(global.units[i]);
    }

    /** Check the result of this phase for the given compilation unit. */
    public void check(CompilationUnit unit) {
        Checker[] checkers = postCheckers(unit.global);
        for (int i = 0; i < checkers.length; i++) checkers[i].traverse(unit);
    }

    /** Returns an array of checkers which can be applied after the phase. */
    public Checker[] postCheckers(Global global) {
        return new Checker[0];
    }

    /** Returns the name of this phase. */
    public final String toString() {
        return descriptor.name();
    }

    //########################################################################
}
