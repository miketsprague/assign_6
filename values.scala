package cs162.miniJS.values

import cs162.miniJS.syntax._
import cs162.miniJS.domains._
import cs162.miniJS.gc._
import Value._
import Domains._

// language values
sealed abstract class Value

// companion object
object Value {

  // implicit conversions
  implicit def v2n( v:NumV )    : BigInt  = v.n
  implicit def v2b( v:BoolV )   : Boolean = v.b
  implicit def v2s( v:StrV )    : String  = v.s
  implicit def n2v( n:BigInt )  : NumV    = NumV( n )
  implicit def b2v( b:Boolean ) : BoolV   = BoolV( b )
  implicit def s2v( s:String )  : StrV    = StrV( s )
  
}

// storable values (non-exceptional)
sealed abstract class Storable extends Value {
  
  def T: Boolean

  def + ( v:Storable ): Storable
  def − ( v:Storable ): Storable
  def × ( v:Storable ): Storable
  def ÷ ( v:Storable ): Storable
  def ≈ ( v:Storable ): Storable
  def ≠ ( v:Storable ): Storable
  def ≤ ( v:Storable ): Storable
  def < ( v:Storable ): Storable
  def ∧ ( v:Storable ): Storable
  def ∨ ( v:Storable ): Storable
  
  def neg: Storable
  def not: Storable
}

// numbers
case class NumV( n:BigInt ) extends Storable {
  def T = n != 0
  
  def + ( v:Storable ) = v match {
    case NumV( _n ) ⇒ n + _n
    case _ ⇒ throw undefined
  }
  def − ( v:Storable ) = v match {
    case NumV( _n ) ⇒ n - _n
    case _ ⇒ throw undefined
  }
  def × ( v:Storable ) = v match {
    case NumV( _n ) ⇒ n * _n
    case _ ⇒ throw undefined
  }
  def ÷ ( v:Storable ) = v match {
    case NumV( _n ) ⇒ if ( _n != 0 ) n / _n else throw undefined
    case _ ⇒ throw undefined
  }
  def ≈ ( v:Storable ) = v match {
    case NumV( _n ) ⇒ n == _n
    case _ ⇒ false
  }
  def ≠ ( v:Storable ) = v match {
    case NumV( _n ) ⇒ n != _n
    case _ ⇒ true
  }
  def ≤ ( v:Storable ) = v match {
    case NumV( _n ) ⇒ n <= _n
    case _ ⇒ throw undefined
  }
  def < ( v:Storable ) = v match {
    case NumV( _n ) ⇒ n < _n
    case _ ⇒ throw undefined
  }
  def ∧ ( v:Storable ) = this.T && v.T
  def ∨ ( v:Storable ) = this.T || v.T
  
  def neg = -n
  def not = !this.T
  
  override def toString = n.toString
}
  
// booleans
case class BoolV( b:Boolean ) extends Storable {
  def T = b
  
  def + ( v:Storable ) = throw undefined
  def − ( v:Storable ) = throw undefined
  def × ( v:Storable ) = throw undefined
  def ÷ ( v:Storable ) = throw undefined
  def ≈ ( v:Storable ) = v match {
    case BoolV( _b ) ⇒ b == _b
    case _ ⇒ false
  }
  def ≠ ( v:Storable ) = v match {
    case BoolV( _b ) ⇒ b != _b
    case _ ⇒ true
  }
  def ≤ ( v:Storable ) = throw undefined
  def < ( v:Storable ) = throw undefined
  def ∧ ( v:Storable ) = this.T && v.T
  def ∨ ( v:Storable ) = this.T || v.T
  
  def neg = throw undefined
  def not = !this.T
  
  override def toString = b.toString
}

// strings
case class StrV( s:String ) extends Storable {
  def T = s != ""
  
  def + ( v:Storable ) = v match {
    case StrV( _s ) ⇒ s + _s
    case _ ⇒ throw undefined
  }
  def − ( v:Storable ) = throw undefined
  def × ( v:Storable ) = throw undefined
  def ÷ ( v:Storable ) = throw undefined
  def ≈ ( v:Storable ) = v match {
    case StrV( _s ) ⇒ s == _s
    case _ ⇒ false
  }
  def ≠ ( v:Storable ) = v match {
    case StrV( _s ) ⇒ s != _s
    case _ ⇒ true
  }
  def ≤ ( v:Storable ) = v match {
    case StrV( _s ) ⇒ s <= _s
    case _ ⇒ throw undefined
  }
  def < ( v:Storable ) = v match {
    case StrV( _s ) ⇒ s < _s
    case _ ⇒ throw undefined
  }
  def ∧ ( v:Storable ) = this.T && v.T
  def ∨ ( v:Storable ) = this.T || v.T
  
  def neg = throw undefined
  def not = !this.T
  
  override def toString = s
}

// undefined value
case class UndefV() extends Storable {
  def T = false
  
  def + ( v:Storable ) = throw undefined
  def − ( v:Storable ) = throw undefined
  def × ( v:Storable ) = throw undefined
  def ÷ ( v:Storable ) = throw undefined
  def ≈ ( v:Storable ) = v match {
    case UndefV() ⇒ true
    case _ ⇒ false
  }
  def ≠ ( v:Storable ) = v match {
    case UndefV() ⇒ false
    case _ ⇒ true
  } 
  def ≤ ( v:Storable ) = throw undefined
  def < ( v:Storable ) = throw undefined
  def ∧ ( v:Storable ) = this.T && v.T
  def ∨ ( v:Storable ) = this.T || v.T
  
  def neg = throw undefined
  def not = !this.T
  
  override def toString = "undef"
}

// closures
case class CloV( xs:Seq[String], t:Term, ρ:Env ) extends Storable {
  def T = true
  
  def + ( v:Storable ) = throw undefined
  def − ( v:Storable ) = throw undefined
  def × ( v:Storable ) = throw undefined
  def ÷ ( v:Storable ) = throw undefined
  def ≈ ( v:Storable ) = false
  def ≠ ( v:Storable ) = true
  def ≤ ( v:Storable ) = throw undefined
  def < ( v:Storable ) = throw undefined
  def ∧ ( v:Storable ) = this.T && v.T
  def ∨ ( v:Storable ) = this.T || v.T
  
  def neg = throw undefined
  def not = !this.T
  
  override def toString = "[closure]"
}

// store locations
case class Address( var loc:Int ) extends Storable {
  def T = σ(this) match {
    case l: ListV => l.T
    case _ => true
  }

  def + ( v:Storable ) = throw undefined
  def − ( v:Storable ) = throw undefined
  def × ( v:Storable ) = throw undefined
  def ÷ ( v:Storable ) = throw undefined
  def ≈ ( v:Storable ) = v match {
    case Address( _loc ) ⇒ loc == _loc
    case _ ⇒ false
  }
  def ≠ ( v:Storable ) = v match {
    case Address( _loc ) ⇒ loc != _loc
    case _ ⇒ true
  }
  def ≤ ( v:Storable ) = throw undefined
  def < ( v:Storable ) = throw undefined
  def ∧ ( v:Storable ) = this.T && v.T
  def ∨ ( v:Storable ) = this.T || v.T

  def neg = throw undefined
  def not = !this.T
  
  override def toString = "a" + loc
}

trait ObjectListNode extends Storable {
  def T = throw undefined
  def + ( v:Storable ) = throw undefined
  def − ( v:Storable ) = throw undefined
  def × ( v:Storable ) = throw undefined
  def ÷ ( v:Storable ) = throw undefined
  def ≈ ( v:Storable ) = throw undefined
  def ≠ ( v:Storable ) = throw undefined
  def ≤ ( v:Storable ) = throw undefined
  def < ( v:Storable ) = throw undefined
  def ∧ ( v:Storable ) = throw undefined
  def ∨ ( v:Storable ) = throw undefined
  
  def neg = throw undefined
  def not = throw undefined
}

// objects are broken up into a series of cons cells so that
// we don't have to allocate big, contiguous chunks.
case class ObjectCons(key: String, value: Storable, next: Address) extends ObjectListNode
case object ObjectNil extends ObjectListNode

// objects
case class ObjectV( head: Address = σ.objectNilAddress ) extends Storable {
  def T = true
  
  def + ( v:Storable ) = throw undefined
  def − ( v:Storable ) = throw undefined
  def × ( v:Storable ) = throw undefined
  def ÷ ( v:Storable ) = throw undefined
  def ≈ ( v:Storable ) = throw undefined
  def ≠ ( v:Storable ) = throw undefined
  def ≤ ( v:Storable ) = throw undefined
  def < ( v:Storable ) = throw undefined
  def ∧ ( v:Storable ) = this.T && v.T
  def ∨ ( v:Storable ) = this.T || v.T
  
  def neg = throw undefined
  def not = !this.T

  // gets Some(the value of the field for this object)
  // or None.  Does not consider the prototype chain.
  def get(s: String): Option[Storable] = {
    def getAddr(a: Address): Option[Storable] =
      σ(a) match {
	case ObjectCons(`s`, v, _) => Some(v)
	case ObjectCons(_, _, next) => getAddr(next)
	case ObjectNil => None
	case _ => println("ADDR: " + a + "\nVALUE: " + σ(a)) ; throw undefined
      }
    getAddr(head)
  }
  
  // field lookup using prototype-based inheritance
  def apply( s:String ): Storable = get(s) match {
    case Some( v ) ⇒ v
    case None ⇒ get("proto") match {
      case None ⇒ UndefV()
      case Some( a: Address ) ⇒ toObj( σ(a) )( s )
      case _ ⇒ throw undefined
    }
  }

  // inserts the given pairing into this map
  // this is done entirely in an immutable fashion
  // returns the new object
  def insert(key: String, value: Storable): ObjectV = {
    // prepare for allocation
    RootSet.pushExtra(head)
    RootSet.pushExtra(value)

    // returns the address of the new cons cell holding
    // the end of the list. Only called when we do
    // already contain it (for performance)
    def ins(a: Address): Address = 
      σ(a) match {
	case ObjectCons(`key`, _, next) => {
	  RootSet.pushExtra(next)
	  val retval = (σ += ObjectCons(key, value, next))
	  RootSet.popExtra()
	  retval
	}
	case ObjectCons(oldKey, oldValue, oldNext) => {
	  RootSet.pushExtra(oldValue)
	  RootSet.pushExtra(oldNext)
	  val rest = ins(oldNext)
	  RootSet.popExtra()
	  RootSet.pushExtra(rest)
	  val retval = (σ += ObjectCons(oldKey, oldValue, rest))
	  RootSet.popExtraTimes(2)
	  retval
	}
	case _ =>
	  throw undefined
      }
    
    val newList = get(key) match {
      case Some(_) => 
	ins(head)
      case None => 
	σ += ObjectCons(key, value, head)
    }
    RootSet.popExtraTimes(2)
    ObjectV(newList)
  }
	      
  def :+( sv:Tuple2[StrV,Storable] ): ObjectV =
    insert(sv._1, sv._2)
}

// lists (lists are immutable, so we don't use Address indirection)
object ListV {
  // makes a cons cell putting the given storable onto the given list
  // returns the address of the new cons cell
  def cons(s: Storable, lst: Address): Address = {
    assert(σ(lst).isInstanceOf[ListV])
    RootSet.pushExtra(s)
    RootSet.pushExtra(lst)
    val retval = (σ += Cell(s, lst))
    RootSet.popExtraTimes(2)
    retval
  }
}

sealed abstract class ListV extends Storable {
  def T = this match {
    case ListNil ⇒ false
    case _ ⇒ true
  }

  def + ( v:Storable ) = throw undefined
  def − ( v:Storable ) = throw undefined
  def × ( v:Storable ) = throw undefined
  def ÷ ( v:Storable ) = throw undefined
  def ≈ ( v:Storable ) = v == this
  def ≠ ( v:Storable ) = v != this
  def ≤ ( v:Storable ) = throw undefined
  def < ( v:Storable ) = throw undefined
  def ∧ ( v:Storable ) = this.T && v.T
  def ∨ ( v:Storable ) = this.T || v.T

  def neg = throw undefined
  def not = !this.T
}

case object ListNil extends ListV                       // empty list
case class Cell(hd:Storable, tl:Address) extends ListV // list cell
