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
  def validAddress(pos: Int): Boolean =
  pos >= 0 && pos < heap.length

  def assertValidAddress(pos: Int) {
    assert(validAddress(pos))
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
     case _ => throw undefined
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
class Freelist(size: Int) extends Heap(size) with DebugTrace {
  // create the list head(s) and write the necessary metadata to show
  // that we have room

  // allocates the given storable, starting from the address of a list head
  // throws OOM if there isn't enough memory
  def allocate(s: Storable, listHead: Int): Address = {

    var finished = false
    var current = listHead
    var previous = current
    while( finished == false){
      if ( validAddress(current) == false){
        finished = true
        throw OOM()
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
              current = next
            }
            else{
             //big enough -- write the information
             writeTo(s,current)

             val dif = (blocks  - allocSize(s)+1)
             if ( dif > 0){
              //make new free block
              heap(current + blocks - dif) = FreeMetadata(dif,next)
             }
             val p = heap(previous)
             heap(previous) = p match{
              case FreeMetadata(blocks2, _) => FreeMetadata(blocks2,current+blocks-diff)
              case _ =>
             }
             finished = true
           }
         }
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

  // takes a set of addresses that are known to be live, along with the address of
  // the list head and the ending address of the space we are collecting
  def collectAllBut(live: Set[Address], listHead: Int, end: Int) {
    var current = listHead
    var previous = current
    while (validAddress(current)){
      val c = heap(current)
      if ( (live contains Address(current))==false){ // dead address
        c match { 
          case AllocatedMetadata(blocks, _) =>{
            heap(previous) match{
              case FreeMetadata(blocks2,_) => heap(previous) = FreeMetadata(blocks2+blocks,current+blocks)
              // coalesce and replace with free block
              case _ => heap(current) = FreeMetadata(blocks,current+blocks)
            }
          }
          case _ =>
        }
        }else{
          c match{
            case AllocatedMetadata(blocks, _) => current = current+blocks
            case FreeMetadata(blocks,next) => {
              current = next
              val p = heap(previous)
              p match{
                case FreeMetadata(blocks2,_) =>  heap(previous) = FreeMetadata(blocks2+blocks,current+blocks)
                // coalesce and replace with free block
                case _ => previous = current
              }
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
