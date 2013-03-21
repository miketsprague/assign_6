package cs162.miniJS.gc

import scala.collection.mutable.{Buffer, HashSet => MSet, Stack => MStack}

import cs162.miniJS.freelist._
import cs162.miniJS.domains._
import Domains._
import cs162.miniJS.values._
import Value._

// out of memory exception
case class OOM() extends Exception( "Out of memory" )

object RootSet {
  // paulot: TODO: add private
  val extra: MStack[Storable] = new MStack()

  def pushExtra(s: Storable) {
    extra.push(s)
  }
  
  def popExtra() {
    extra.pop()
  }

  def popExtraTimes(n: Int) {
    (0 until n).foreach(_ => popExtra())
  }

  def apply(): Set[Storable] =
    ρ.rootSet ++ extra.toSet
}

object DebugTrace {
  var tracep = false
}
trait DebugTrace {
  def trace(msg : => String) = if (DebugTrace.tracep) println(msg)
}

sealed trait Collector extends HeapInterface with DebugTrace {
  def gcAlloc(s: Storable): Address
  def gcRead(a: Address): Storable
  def gcModify(a: Address, v: Storable) {
    // done via emulation - alloc again and update the address
    RootSet.pushExtra(v)
    val newAddr = gcAlloc(v)
    RootSet.popExtra()
    a.loc = newAddr.loc
  }
  def AddressValidAddress(a: Address): Boolean ={
    trace(" this is a :"+a)
    if ( a!=null ) validAddress(a.loc) else false
  }
  def assertValidAddress(a: Address) {
    assert(AddressValidAddress(a))
  }
}

class StubCollector(max: Int) extends Heap(max) with Collector {
  private var nextAddress = 0

  def gcAlloc(s: Storable): Address = {
    trace("## gcAlloc: allocating space for " + s)
    val size = allocSize(s) + 1 // + 1 for metadata
    if (validAddress(nextAddress) && validAddress(nextAddress + size)) {
      val a = nextAddress
      writeTo(s, a)
      nextAddress += size
      trace("## gcAlloc: allocated space for " + s + " at address " + a)
      Address(a)
    } else {
      throw OOM()
    }
  }
    
  override def gcRead(a: Address): Storable = {
    trace("Attempting to read from address " + a)
    readFrom(a.loc)
  }
}

trait TracingCollector extends Collector {

  override def gcRead(a: Address): Storable = {
    trace("Attempting to read from address " + a)
    readFrom(a.loc)
  }

  // obj: the object to deeply search for references
  // seen: (rootset) set of already seen objects
  // returns: unique set of addresses that are references by obj
  def extract(obj: Storable, seen: MSet[Address]): MSet[Address] = {
       val trav: MSet[Address] = obj match {
            case addr @ Address(a) => {
                println("addrs " + addr)
                MSet(addr) ++ extract(gcRead(addr),seen+addr)
            }
            case ObjectV(a) => {
                println("object " + a)
                if (seen contains a) 
                    MSet(a)
                else {
                    MSet(a) ++ extract(gcRead(a),seen+a)
                }
            }
            case Cell(hd,tl) => {
                println("cell " + tl)
                if (seen contains tl) 
                    MSet(tl)
                else {
                    MSet(tl) ++ extract(gcRead(tl),seen+tl)
                }
            }
            case ObjectCons(key,value,next) => {
                println("object cons " + next)
                if (seen contains next) 
                    MSet(next) 
                else { 
                    MSet(next) ++ extract(value, seen+next) ++ extract(gcRead(next),seen+next)
                }
            }
            case _ => { MSet() }

       }
       trav
  }

  // Returns a set of items that are reachable
  def traceReachable(): Set[Address] = {
    val roots = rootSet()
    println("root set " + roots)
    var seen: MSet[Address] = new MSet()
    
    roots.foreach(root => {
    	seen = seen ++ extract(root,seen)
      println("seen " + seen)
    })
    
    //var ret: Set[Address] = Set()
    //seen.foreach( a => ret += a)
    //ret
    seen.toSet
    
    // ---FILL ME IN---
    //
    // HINTS:
    // Recurively trace through the roots.  If an address is found that doesn't
    // already exist in the addresses we've already seen, then traverse it by
    // reading in the object from the store.
  }

  def rootSet(): Set[Storable] = RootSet()
}
  
// semispace collector
// basic idea: split the heap into two halves: from-space and to-space
// Allocate into the to-space.  When the to-space becomes full, copy
// live objects into the from-space.  Once that is complete, swap the
// from and to spaces.
// DONE!
class SemispaceCollector(max: Int) extends Heap(max) with TracingCollector {
  if (max % 2 != 0) {
    throw undefined
  }
  
  var bumpPointer = 0
  var toStart = 0
  var fromStart = max / 2

  def swapSpaces() {
    val temp = fromStart
    fromStart = toStart
    toStart = temp
    bumpPointer = toStart
  }

  def checkMetadata(a: Address): Boolean = {
      heap(a.loc) match {
          case AllocatedMetadata => true
          case FreeMetadata      => false
          case _                 => false
      }
  }

  def gcAlloc(s: Storable): Address = {
      printHeap()
      trace("## gcAlloc: allocating space for " + s)
      val size = allocSize(s) + 1           // + 1 for metadata
      var top  = bumpPointer > max / 2
      bumpPointer += size                   // Increment bp by block size
      if ((top && bumpPointer > max) || (!top && bumpPointer > max / 2)) {  // No more room left
          trace("## gcAlloc: Memory full")
          doGC()
          top  = bumpPointer > max / 2
          bumpPointer += size                   // Increment bp by block size
          if ((top && bumpPointer > max) || (!top && bumpPointer > max / 2)) {  // No more room left
              throw OOM()
          } else {
              trace("## gcAlloc: writing object " + s + " at address " + (bumpPointer - size))
              // write object
              writeTo(s, bumpPointer - size)
              Address(bumpPointer - size)
          }
      } else {
          trace("## gcAlloc: writing object " + s + " at address " + (bumpPointer - size))
          // write object
          writeTo(s, bumpPointer - size)
          Address(bumpPointer - size)
      }

    // ---FILL ME IN---
    //
    // HINTS:
    // You'll need to increment the bump pointer by the size of the block.
    // Note again that allocSize does not account for the metadata.
    // If there isn't enough room left, then you'll have to do GC and try again.
    // If there still wasn't enough room after GC, the program should terminate
    // abnormally with an OOM error.
  }
  
  def doGC() {
    trace("## doGC: Garbage collecting")
    swapSpaces()
    val live = traceReachable()
    trace("LIVE: " + live.toString())
    trace("ROOT: " + RootSet.extra.toString())
    printHeap()
    live.foreach(a => {
      // ---FILL ME IN---
      // newAddr should be an address corresponding to the new address of the object
      // in the new to-space
      trace("## doGC: got new address at " + bumpPointer)
      val newAddr: Address = Address(bumpPointer)
      // copy the old object to the newAddr
     // if (checkMetadata(a)) {
          trace("## doGC: Metadata checks out!")
          trace("## doGC: reading old object from  " + a.loc)
          val obj = readFrom(a.loc)
          trace("## doGC: writing old object at " + bumpPointer)
          writeTo(obj, bumpPointer)
          bumpPointer += allocSize(obj) + 1     // +1 for metadata
          a.loc = newAddr.loc
     // } else {
     //     trace("## doGC: Metadata does NOT check out! BOOO")
     // }
    })
  }

  override def gcRead(a: Address): Storable = {
    trace("Attempting to read from address " + a)
    readFrom(a.loc)
  }
} // SemispaceCollector


class MarkSweepCollector(max: Int) extends Freelist(max) with TracingCollector {
  override def gcRead(a: Address): Storable = {
    trace("Attempting to read from address " + a)
    readFrom(a.loc)
  }

  def gcAlloc(s: Storable): Address = {
    var done = false
    var a : Address = null
    while(!done){
      try{
        a = allocate(s,0)
        done = true
      } catch {
        case OOM() => { 
          println("before gc")
          printHeap()
          doGC() //if we get OOM, do a gc
          println("after gc")
          printHeap()
          a = allocate(s,0)
          done = true
        }
        case w @ _ => throw w
      }
    } 
    a
    // ---FILL ME IN---
    // Freelist's allocate function does the bulk of the work.  The important bit
    // is that GC needs to be triggered if allocate threw an OOM
  }

  def doGC() {
    val live = traceReachable()
    collectAllBut(live,0,max)

    // ---FILL ME IN---
    // Freelist's collectAllBut function does exactly what you need here; it's‌
    // just a matter of passing the right parameters
    trace("## gcAlloc: GC complete, found " + live.size + " live objects")
  }
}

class GenerationalCollector(val nurserySize: Int, val tenuredSize: Int) 
extends Freelist(nurserySize + tenuredSize) with TracingCollector {
  val backPointers: Buffer[Address] = Buffer()
  // HINTS:
  // 1.) There are a number of ways to implement the generational collector.
  //     However, the simplest will probably involve reusing a lot of the same 
  //     components you've already developed and tested.  A fairly simple way 
  //     is to have multiple list heads in the free list: one for the nursery,
  //     and the other for the tenured space.  For the most part, these can be
  //     treated as separate spaces, except at the all-important moment when we
  //     need to do a major GC.
  // 2.) backPointers holds addresses that need to be traced.  This happens
  //     whenever something in the tenured heap is modified to point to something
  //     in the nursery.  Under these conditions, it's possible for something
  //     that's actually reachable to not be included in the root set for minor
  //     GC, since these references are from the tenured heap.  This is
  //     intentionally not actually a Set, since if the underlying address changes
  //     (as with an update) then we will invalidate the properties needed for
  //     objects to be in hash sets.

  def gcAlloc(s: Storable): Address = {
    // ---FILL ME IN---
    //
    // HINTS:
    // Try to allocate in the nursery.  If there isn't enough room there, then
    // do a minor GC.  Minor GC could trigger major GC if there isn't enough
    // room to copy live things from the nursery over to the tenured heap
    null
  }

  override def gcRead(a: Address): Storable = {
    trace("## gcRead: Attempting to read from address " + a)
    readFrom(a.loc)
  }

  // There is a lot going on in gcModify.  Point by point:
  // 1.) If we are attempting to modify an address that exists
  //     on the tenured heap, then we need to make a back pointer.  The
  //     value (v) could reach a reference, either directly or indirectly,
  //     to the nursery.
  // 2.) We circumvent the usual gcAlloc if something is tenured, and
  //     alloc directly in the tenured heap.  This breaks the usual rules.
  //     This is needed since, because of issues like backpointers, something
  //     that has become tenured must remain tenured for correctness.
  // 3.) One may ask: why not modify the address directly, and avoid all
  //     the extra trouble?  Unfortunately, this is not possible in general
  //     with our language.  If we want to overwrite with something that
  //     was larger than what was there to begin with, almost assurredly there
  //     won't be enough room at the exact same spot.  This is why we must
  //     allocate elsewhere and update addresses to reflect the change.
  override def gcModify(a: Address, v: Storable) {
    if (inTenured(a)) {
      RootSet.pushExtra(v)
      backPointers += a
      try {
	val newAddr = allocateInTenured(v)
	a.loc = newAddr.loc
      } catch {
	case _: OOM => {
	  majorGC()
	  val newAddr = allocateInTenured(v)
	  a.loc = newAddr.loc
	}
      } finally {
	RootSet.popExtra()
      }
    } else {
      super.gcModify(a, v)
    }
  }

  def allocateInNursery(s: Storable): Address = null
    // ---FILL ME IN---
    // Call Freelist's allocate method, using the freelist head specifically
    // for the nursery

  def allocateInTenured(s: Storable): Address = null
    // ---FILL ME IN---
    // Call Freelist's allocate method, using the freelist head specifically
    // for the tenured heap

  // may trigger major GC
  def minorGC() {
    // ---FILL ME IN---
    //
    // HINTS:
    // Trace the nursery and the back pointers.  Copy any live objects in
    // the nursery to the tenured heap.  In the process of copying, you may
    // find that there is no room left in the tenured heap.  If this is the case,
    // then perform full GC.  Once you're done copying objects from the nursery,
    // make sure to reset the freelist to reflect the fact that the nursery
    // is open again.  Make sure you update object addresses when they are
    // moved.
    //
    // In order to perform tracing of the nursery separately, you may need to
    // override the definition of TracingCollector's rootSet() function.
    //
    // Once minor GC is overwith, you should empty out the backPointers set,
    // since everything should be in the tenured heap anyway at that point.
  }

  def inNursery(a: Address): Boolean = false
    // ---FILL ME IN---
    // Return true if the address exists in the nursery, else false.
    // This should only take a single line

  def inTenured(a: Address): Boolean = false
    // ---FILL ME IN---
    // Return true if the address exists in the tenured heap, else false.
    // This should only take a single line


  def majorGC() {
    // ---FILL ME IN---
    //
    // HINTS:
    // Performs a full GC across both the nursery and the tenured heap.
    // Live objects in the nursery still need to be moved over to the
    // tenured heap.  At the end of full GC, the nursery should be empty,
    // just like with minor GC.  Be sure to update object addresses
    // when they are moved.
  }
}
