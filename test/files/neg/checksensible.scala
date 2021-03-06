final class Bip { def <=(other: Bop) = true }
final class Bop { }
object Bep { }

final class Zing {
  def !=(other: Zing) = false
}

// 7 warnings
class RefEqTest {
  object Shmoopie
  
  (new AnyRef) eq (new AnyRef)
  (new AnyRef) ne (new AnyRef)
  Shmoopie eq (new AnyRef)
  (Shmoopie: AnyRef) eq (new AnyRef)
  (new AnyRef) eq Shmoopie
  (new AnyRef) eq null
  null eq new AnyRef
}

// 11 warnings
class EqEqValTest {
  var c = 0
  
  (c = 1) == 0
  0 == (c = 1)
  
  1 == "abc"
  "abc" == 1  // doesn't warn since String defines an equals method
  
  new AnyRef == 1
  1 == new AnyRef
  
  1 != true
  () == true
  () == ()
  () == println
  
  (1 != println)
  (1 != 'sym)
}

// 12 warnings
class EqEqRefTest {  
  val ref = new Bop
  ((x: Int) => x + 1) == null
  Bep == ((_: Int) + 1)

  new Object == new Object
  new Object == "abc"
  new Exception() != new Exception()

  val foo: Array[String] = Array("1","2","3")
  if (foo.length == null) "plante" else "plante pas"

  // final classes with default equals
  val x1 = new Bip
  val x2 = new Bop  
  (x1 == x2)
  
  class C1 { }
  class C2 extends C1 { }
  final class Z1 extends C2 { }
  final class C3 extends C2 { def !=(other: Z1) = false }
  val z1 = new Z1
  val c3 = new C3

  // all but c3 != z1 should warn
  c3 != z1
  c3 == z1
  z1 == c3
  z1 != c3
  c3 != "abc"
  
  // non-warners
  (null: AnyRef) == (null: AnyRef)
  (x1 <= x2)
  
  def main(args: Array[String]) = {
    val in = new java.io.FileInputStream(args(0))
    var c = 0    
    while ((c = in.read) != -1)
      print(c.toChar)

    in.close
  }
}
