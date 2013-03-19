package cs162.miniJS.domains

import scala.io._
import scala.collection.mutable.{ Map => MMap }
import cs162.miniJS.syntax._
import cs162.miniJS.values._
import cs162.miniJS.gc._
import Value._
import Domains._

object Domains {

  // exception for impossible things
  object inconceivable extends Exception
  
  // exception for undefined behavior
  object undefined extends Exception
  
  // take a Storable guaranteed to be an ObjectV and downcast it
  def toObj( v:Storable ): ObjectV =
    v match {
      case obj:ObjectV ⇒ obj
      case _ ⇒ throw inconceivable
    }

}

// global store
object σ {
  var gc: Collector = null
  var objectNilAddress: Address = null
  var listNilAddress: Address = null

  def init() {
    objectNilAddress = (this += ObjectNil)
    RootSet.pushExtra(objectNilAddress)
    listNilAddress = (this += ListNil)
    RootSet.pushExtra(listNilAddress)
  }

  def apply( a:Address ): Storable = 
    gc.gcRead(a)

  def update( a:Address, v:Storable ): Storable = {
    gc.gcModify(a, v)
    UndefV()
  }

  def +=( v:Storable ): Address = 
    gc.gcAlloc(v)

  def ++=( vs:Seq[Storable] ): Seq[Address] = 
    vs.map(this += _)
}

// global environment
import scala.collection.mutable.{Stack => MStack}
case class Env(env: MStack[(String, Address)] = new MStack()) {
  def apply(x: String): Address =
    env.find(_._1 == x).map(_._2).getOrElse(throw undefined)

  def apply(x: Var): Address =
    apply(x.x)

  def push(binding: (String, Address)) {
    env.push(binding)
  }
  
  def pop(): (String, Address) =
    env.pop()

  def popTimes(n: Int) {
    (0 until n).foreach(_ ⇒ pop())
  }

  def ++(bindings: Seq[(String, Address)]) {
    bindings.foreach(push)
  }

  def rootSet(): Set[Address] =
    env.map(_._2).toSet
}

object ρ extends Env()
