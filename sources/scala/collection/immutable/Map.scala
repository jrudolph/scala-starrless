/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.collection.immutable;


/** This trait extends the Map interface of collections that unambiguously map
 *  keys to values (i.e. a key is mapped to at least one value).
 *  This trait defines the interface for functional map implementations 
 *  relying on immutable data structures.
 *  Concrete map implementations have to provide functionality for the 
 *  abstract methods in scala.collection.Map as well as for 
 *  <code>factory</code>, <code>update</code>, and -.
 *
 *  @author  Matthias Zenger
 *  @author  Erik Stenman
 *  @version 1.1, 22/03/2004
 */
trait Map[A, B] with scala.collection.Map[A, B] {

    /** This method returns a new map instance of the same class
     *  mapping keys of the same type to values of type <code>C</code>.
     */
    def empty[C]: Map[A, C];
    
    /** This method allows one to create a new map with an
     *  additional mapping from <code>key</code>
     *  to <code>value</code>. If the map contains already a
     *  mapping for <code>key</code>, it will be overridden by this
     *  function.
     */
    def update(key: A, value: B): Map[A, B];
  
    /** This creates a new mapping without the given <code>key</code>.
     *  If the map does not contain a mapping for the given key, the
     *  method returns the same map.
     */
    def -(key: A): Map[A, B];
  
    /** This method defines syntactic sugar for adding a
     *  mapping. It is typically used in the following way:
     *  <pre>
     *  map + key -> value;
     *  </pre>
     */
    def +(key: A): MapTo = new MapTo(key);
    
    /** <code>incl</code> can be used to add many mappings at the same time
     *  to the map. The method assumes that a mapping is represented
     *  by a <code>Pair</code> object who's first component denotes the
     *  key, and who's second component refers to the value.
     */
    def incl(mappings: Pair[A, B]*): Map[A, B] = incl(mappings);
  
    /** <code>incl</code> can be used to add many mappings at the same time
     *  to the map. The method assumes that each mapping is represented
     *  by an Iterator over <code>Pair</code> objects who's first component 
     *  denotes the key, and who's second component refers to the value.
     */    
    def incl(map: Iterable[Pair[A, B]]): Map[A, B] = {
        val iter = map.elements;
        var res = this;
        while (iter.hasNext) {
            val Pair(key, value) = iter.next;
            res = res.update(key, value);
        }
        res
    }
    
    /** This method will return a map where all the mappings 
     *  for the given sequence of keys are removed from the map.
     */
    def excl(keys: A*): Map[A, B] = excl(keys);
  
    /** This method removes all the mappings for keys provided by an
     *  iterator over the elements of the <code>keys</code> object.
     */
    def excl(keys: Iterable[A]): Map[A, B] = {
        val iter = keys.elements;
        var res = this;
        while (iter.hasNext) {
            res = res - iter.next;
        }
        res;
    }
    
    /** This function transforms all the values of mappings contained
     *  in this map with function <code>f</code>.
     */
    def map[C](f: (A, B) => C): Map[A, C] = {
        var res = empty[C];
        elements foreach {
            case Pair(key, value) => res = res.update(key, f(key, value));
        }
        res;
    }
    
    /** This method removes all the mappings for which the predicate
     *  <code>p</code> returns <code>false</code>.
     */
    def filter(p: (A, B) => Boolean): Map[A, B] = {
        var res = this;
        toList foreach {
            case Pair(key, value) => if (!p(key, value)) { res = res.excl(key); }
        }
        res;
    }
    
    /** Returns a string representation of this map which shows
     *  all the mappings.
     */
    override def toString() =
        if (size == 0)
            "{}"
        else
            "{" + {
                val iter = elements;
                var res = mappingToString(iter.next);
                while (iter.hasNext) {
                    res = res + ", " + mappingToString(iter.next);
                }
                res;
            } + "}";

    override def hashCode() = {
      elements.foldLeft(0)((hash:Int,pair:Object) => hash + pair.hashCode());
    }
  
    /** This method controls how a mapping is represented in the string
     *  representation provided by method <code>toString</code>.
     */
    def mappingToString(p: Pair[A, B]) = p._1.toString() + " -> " + p._2;
    
    class MapTo(key: A) {
        def ->(value: B) = update(key, value);
    }
}
