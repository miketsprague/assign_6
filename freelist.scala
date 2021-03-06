package cs162.miniJS.freelist

import cs162.miniJS.syntax._
import cs162.miniJS.gc._
import cs162.miniJS.domains._
import Domains._
import cs162.miniJS.values._
import Value._

// needed as a workaround to type erasure
case class StringSeq(strings: Seq[String])

object StorableType extends Enumeration {
  type StorableType = Value
  val NumVType, BoolVType, StrVType, UndefVType, CloVType, AddressType,
  ObjectConsType, ObjectNilType, ObjectVType, ListNilType, CellType = Value

  def getType(s: Storable): StorableType =
  s match {
    case _: NumV => NumVType
    case _: BoolV => BoolVType
    case _: StrV => StrVType
    case _: UndefV => UndefVType
    case _: CloV => CloVType
    case _: Address => AddressType
    case _: ObjectCons => ObjectConsType
    case ObjectNil => ObjectNilType
    case _: ObjectV => ObjectVType
    case ListNil => ListNilType
    case _: Cell => CellType
  }
}
import StorableType._

trait HeapInterface extends DebugTrace {
  protected def heap: Array[Any]
  def allocSize(s: Storable): Int =
  s match {
    case NumV(_) | BoolV(_) | StrV(_) | ObjectV(_) | UndefV() | ListNil | Address(_) | ObjectNil => 1
    case Cell(_, _) => 2
    case CloV(_, _, _) | ObjectCons(_, _, _) => 3
  }

  // NOTE: validAddress is called by many things
  // you may want to make this more specific for your own GC.  As
  // defined, this is pretty lenient
  def validAddress(pos: Int, max: Int = heap.length): Boolean = {
  pos >= 0 && pos < max
  } 
  def assertValidAddress(pos: Int, max: Int = heap.length) {
    assert(validAddress(pos,max))
  }

  val deserialize: Map[StorableType, (Int, PartialFunction[List[Any], Storable])] =
  Map(NumVType -> (1, { case List(n: NumV) => n }),
   BoolVType -> (1, { case List(b: BoolV) => b}),
   StrVType -> (1, { case List(s: StrV) => s}),
   UndefVType -> (1, { case List(u: UndefV) => u}),
   (CloVType, (3, ({ case List(xs: StringSeq, t: Term, ρ: Env) => CloV(xs.strings, t, ρ) }))),
   AddressType -> (1, { case List(a: Address) => a}),
   ObjectConsType -> (3, { case List(key: String, value: Storable, next: Address) => ObjectCons(key, value, next) }),
   ObjectNilType -> (1, { case List(ObjectNil) => ObjectNil }),
   ObjectVType -> (1, { case List(a: Address) => ObjectV(a) }),
   ListNilType -> (1, { case List(ListNil) => ListNil }),
   CellType -> (2, { case List(hd: Storable, tl: Address) => Cell(hd, tl) }))

  def heapRead(base: Int, numItems: Int): List[Any] = {
    assertValidAddress(base)
    assertValidAddress(base + numItems - 1)
    (base until (base + numItems)).map(heap(_)).toList
  }

  // Handles all deserialization work.  Takes an address of something
  // holding AllocatedMetadata
  def readFrom(pos: Int): Storable = {
    assertValidAddress(pos)
    heap(pos) match {
      case AllocatedMetadata(_, typ) => {
       val (numArgs, constructor) = deserialize(typ)
       val items = heapRead(pos + 1, numArgs)
       assert(constructor.isDefinedAt(items))
       constructor(items)
     }
     case a @ _ => { trace("pos " + pos + " is a " + a + "!!!"); throw undefined }
   }
 }

 val serialize: Map[StorableType, PartialFunction[Storable, List[Any]]] =
 Map(NumVType -> { case n: NumV => List(n) },
   BoolVType -> { case b: BoolV => List(b) },
   StrVType -> { case s: StrV => List(s) },
   UndefVType -> { case u: UndefV => List(u) },
   CloVType -> { case CloV(xs, t, ρ) => List(StringSeq(xs), t, ρ) },
   AddressType -> { case a: Address => List(a) },
   ObjectConsType -> { case ObjectCons(key, value, next) => List(key, value, next) },
   ObjectNilType -> { case ObjectNil => List(ObjectNil) },
   ObjectVType -> { case ObjectV(head) => List(head) },
   ListNilType -> { case ListNil => List(ListNil) },
   CellType -> { case Cell(hd, tl) => List(hd, tl) })

  // handles all serialization work.  Writes metadata at the given position,
  // followed by the serialized object.  This assumes that it's safe to perform
  // this write
  def writeTo(s: Storable, pos: Int){
    assertValidAddress(pos)
    val typ = getType(s)
    val serializer = serialize(typ)
    assert(serializer.isDefinedAt(s))
    val items = serializer(s)
    assertValidAddress(pos + items.size)
    heap(pos) = AllocatedMetadata(items.size + 1, typ)
    items.foldLeft(pos + 1)((curPos, curItem) => {
      heap(curPos) = curItem
      curPos + 1
      })
  }

  // useful for debugging purposes
  def printHeap() {
    trace("HEAP:\n" + heap.toSeq)
  }
}

class Heap(size: Int) extends HeapInterface {
  protected val heap = new Array[Any](size)
}

// blocksAllocated includes the metadata
case class AllocatedMetadata(blocksAllocated: Int, typ: StorableType)

// blocksAvailable includes the metadata
case class FreeMetadata(blocksAvailable: Int, next: Int)

// IMPORTANT NOTES:
// 1.) There are two kinds of blocks: allocated and free.  Both have a field
//     for recording the number of units the block consumes in the heap, which
//     includes the metadata at the start of a block.
// 2.) Allocated blocks start with an AllocatedMetadata object, followed by
//     the serialized version of the object.  Use Heap's readFrom and writeTo
//     methods for manipulating this information
//  Incremdent the size by one to make room for the metadata
class Freelist(size: Int) extends Heap(size+1) with DebugTrace {
  // create the list head(s) and write the necessary metadata to show
  // that we have room .
  heap(0) = FreeMetadata(size+1, -1)

  // allocates the given storable, starting from the address of a list head
  // throws OOM if there isn't enough memory
  def allocate(s: Storable, listHead: Int, end: Int = heap.length): Address = {
    var current = listHead
    // Previous free block
    var previous = -1

    while(true){
      if ( validAddress(current+allocSize(s),end) == false){
        throw OOM()
        //return Address(-1) // this line shouldn't get hit.
      }else{
        heap(current) match {
          case AllocatedMetadata(blocks, _ ) => {
            current = current+blocks
          }
          case FreeMetadata(blocks, next) => {
            //check if its big enough
            if (blocks < allocSize(s)+1) {
              //not big enough -- keep looking
              previous = current
              current = next // go one by one
              if(current < 0) {
                throw OOM() // we're at the end of the list.
              }
            } else{
               //big enough -- write the information
               trace("writing " + s + " at index " + current)
               writeTo(s,current)

               val dif = (blocks  - (allocSize(s)+1))
               if ( dif > 0){
                // Update the free block
                val freeLoc = current + allocSize(s) + 1

                heap(freeLoc) = FreeMetadata(dif, next)

                // If there was a previous free block, we want to make it point to the new free block
                val p = if (previous > -1) heap(previous) else None
                p match{
                  case FreeMetadata(blocks2, _) => { 
                    // Update the old pointer to point to us for free memory
                    trace("updating the old free meta data to point to us...")
                    heap(previous) = FreeMetadata(blocks2,freeLoc) 
                  }
                  case _ => 
                } 
              }
              // Doesn't matter since we're ending.
              trace("Creating object at index " + current + " for addr " + Address(current))
              return Address(current)
            }
          }
          case _ => { /*println("Hit null!  This shouldn't ever happen!");*/ throw OOM() }
        }
      }
    }
    // ---FILL ME IN---
    //
    // HINTS:
    // 1.) You need to find the first free block that has enough room for the storable.
    //     The allocSize function can be used for this.  allocSize does NOT account
    //     for the unit needed by the metadata.
    // 2.) In order to perform the allocation, you'll need to modify the freelist so that
    //     the next reference of the previous free block actually points to the
    //     next available free block. Depending on whether or not there is some space
    //     available in the block after allocation, you may need to create a new
    //     free block
    null
  }

  // msprague:
  // try to coalesce two blocks of memory given a previous location and a current location.
  // return true if we coalesced and false if we didn't
  def attemptCoalesce(current : Int, previous: Int) : Boolean = {
    val currentData = if(validAddress(current)) heap(current) else None
    val previousData = if(validAddress(previous)) heap(previous) else None

    // We can only coalesce if we have two adjacent free blocks
    currentData match {
      case FreeMetadata(blockSize, nextFree) => { 
        previousData match {
          case FreeMetadata(prevSize, _) => {
                heap(previous) = FreeMetadata(prevSize+blockSize, nextFree)
                // Clear ourself (does this matter?) <- No
                heap(current) = Nil 
                // Yo dawg, I heard you like garbage collectors
                // so we wrote a garbage collector who's garbage is collected by the JVM through scala to collect your garbage
                // ^ I suck.  That didn't work at all.
                return true
          }
          case _ => { return false }
        }
      }
      case _ => { return false }
    }

    return false // should be unreachable
  }

  // takes a set of addresses that are known to be live, along with the address of
  // the list head and the ending address of the space we are collecting
  def collectAllBut(live: Set[Address], listHead: Int, end: Int) {
    var current = listHead
    //println("Cleaning all memory except for " + live + " starting at " + listHead + "and ending at " + end)
    trace("Cleaning all memory except for " + live + " starting at " + listHead + "and ending at " + end)
    // Previous keeps track of the last block with metadata.
    // Not the last free block!
    // This is because we want to see if we can use previous to expand (coalesce)
    var previous = -1
    // Go from our head to the end.
    while (current < end){
      // Get the object at index current from the heap.
      val c = heap(current)
      // If we don't want to save this address
      if (!(live contains Address(current))){ 
        c match { 
          // If it's an allocated block, we want to make it a free block.
          case AllocatedMetadata(blocks, _) =>{
            // XXX How do we know the next free block location?
            heap(current) = FreeMetadata(blocks,current+blocks)  
            // If the last block is also free, we want to combine them.
            if(!attemptCoalesce(current, previous)) {
              // Didn't coalesce, update the previous free location to our spot
              previous = current
            }

            current = current + blocks
          }
          case FreeMetadata(blocks, _) => { 
            if(!attemptCoalesce(current, previous)) {
              previous = current
            }
            current = current + blocks
          }
          case a @ _ => { trace("Hit unrecognized object." + a + "  Most likely, this should be the last index..." + current); current = current+1}
        }
      }else{
          c match{
            // Move along, sir.
            // We don't do anything if its allocated and we want to keep it!
            case AllocatedMetadata(blocks, _) => { previous = current; current = current+blocks }
            // Otherwise, it's free.  Try to coalesce it.
            case FreeMetadata(blocks,next) => {
              if(!attemptCoalesce(current, previous)) {
                previous = current
              }
              current = current + blocks
            }
          }
        }
      }




    // ---FILL ME IN---
    //
    // HINTS:
    // 1.) Go over each block, replacing it with a free block if it's currently an allocated
    //     block that doesn't exist in the set of live addresses.
    // 2.) You MUST also implement coalescing as part of this function.  In other words,
    //     if two free blocks end up being side by side, then they should be combined into
    //     a single free block.  You may do this after you've replaced the dead objects with
    //     free blocks, if you wish.  It's also possible to do this simultaneously with
    //     replacing dead objects with free blocks.
  }
}
