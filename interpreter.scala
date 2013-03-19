import scala.io._

import cs162.miniJS.syntax._
import cs162.miniJS.domains._
import cs162.miniJS.values._
import cs162.miniJS.gc._
import Value._
import Domains._

// the main entry point
object miniJS {

  // returns a mapping of args that start with - to the next value
  // special keys are "PROG" and "TRACE"
  def parseArgs(args: Array[String]): Map[String, String] = {
    def parse(l: List[String], accum: Map[String, String]): Map[String, String] =
      l match {
	case Nil => accum
	case prog :: Nil => accum + ("PROG" -> prog)
	case "-trace" :: rest => 
	  parse(rest, accum + ("TRACE" -> ""))
	case key :: value :: rest if key.startsWith("-") =>
	  parse(rest, accum + (key -> value))
	case _ :: rest => parse(rest, accum)
      }
    parse(args.toList, Map())
  }

  def usage() {
    println("Usage: scala miniJS -gc (stub|semi|mark|gen) -size <heap size> [-tenured_size <heap size>] [-trace] program.not")
    println("-tenured_size is applicable only for generational GC; it defaults to 4X the normal size")
    println("-trace will turn on tracing information")
    System.exit(1)
  }

  def isPositiveInt(s: String): Boolean =
    try {
      Integer.parseInt(s) > 0
    } catch {
      case _: NumberFormatException => false
    }

  def validateArgs(args: Map[String, String]) {
    if (!args.contains("-gc")) {
      println("Needs a GC strategy.")
      usage()
    }
    if (!args.contains("-size")) {
      println("Needs a heap size")
      usage()
    }
    if (!isPositiveInt(args("-size"))) {
      println("Heap size must be a positive integer")
      usage()
    }
    if (args.contains("-tenured_size") && args("-gc") != "gen") {
      println("-tenured_size is relevant only to generational GC")
      usage()
    }
    if (args.contains("-tenured_size") && !isPositiveInt(args("-tenured_size"))) {
      println("-tenured_size must be a positive integer")
      usage()
    }
    if (!args.contains("PROG")) {
      println("Missing program name")
      usage()
    }
    if (!Set("stub", "semi", "mark", "gen").contains(args("-gc"))) {
      println("Invalid GC strategy: " + args("-gc"))
      usage()
    }
  }

  def main(args:Array[String]) {
    val argMap = parseArgs(args)
    validateArgs(argMap)
    argMap.get("TRACE").map(_ => DebugTrace.tracep = true)
    val size = argMap("-size").toInt
    val gc = argMap("-gc") match {
      case "stub" => new StubCollector(size)
      case "semi" => new SemispaceCollector(size)
      case "mark" => new MarkSweepCollector(size)
      case "gen" => new GenerationalCollector(
	size,
	argMap.get("-tenured_size").map(_.toInt).getOrElse(size * 4))
      case _  =>  throw new Exception() // shouldn't be possible
    }
    σ.gc = gc
    σ.init()

    // parse the given program
    val ast = ParseL.getAST( Source.fromFile( argMap("PROG") ).mkString )
    val goAdr = σ += ObjectV() // global object (passed to non-methods)
    ρ.push("gObj" → goAdr)
    Interpreter.eval(ast)
    ρ.pop()
  }
}

object Interpreter {
  // adds bindings between the given variables and storables
  // to both the environment and store.  Returns the addresses used.
  // Needed since we could do GC in between
  def addBindings(bindings: Seq[(String, Storable)]): Seq[Address] = 
    bindings.map(pair ⇒ {
      val ref = (σ += pair._2)
      ρ.push(pair._1 → ref)
      ref
    })
  
  // gets the environment to use for the given function
  def closureEnv( fun: Fun ) =
    pruneEnv( fv( fun ) )

  // create an environment containing only variables in the given expression
  def pruneEnv( vars: Set[ Var ] ) = {
    val retval = Env()
    vars.foreach( v => 
      ρ.env.find( _._1 == v.x ) match {
        case Some( b ) => retval.push( b )
        case _ => ()
      } )
    retval
  }

  // get the free variables in the given expression
  def fv( t: Term ): Set[ Var ] = {
    def fvSeq( es: Seq[ Term ] ) =
      es.foldLeft( Set[ Var ]() )( _ ++ fv( _ ) )

    t match {
      case _: Num | _: Bool | _: Str | _: Undef | _: In => Set()
      case v: Var => Set( v )
      case Fun( xs, t ) => fv( t ) -- xs.toSet
      case Let( xs, t ) => fv( t ) -- xs.toSet
      case Then( ts ) => fvSeq( ts )
      case Assign( x, e ) => fv( e ) + x
      case While( e, t ) => fv( e ) ++ fv( t )
      case Output( e ) => fv( e )
      case Update( e1, e2, e3 ) => fv( e1 ) ++ fv( e2 ) ++ fv( e3 )
      case UnOp( _, e ) => fv( e )
      case BinOp( _, e1, e2 ) => fv( e1 ) ++ fv( e2 )
      case If( e, t1, t2 ) => fv( e ) ++ fv( t1 ) ++ fv( t2 )
      case Call( ef, es ) => fv( ef ) ++ fvSeq( es )
      case MCall( e1, e2, es ) => fv( e1 ) ++ fv( e2 ) ++ fvSeq( es )
      case Obj( fbs ) => fvSeq( fbs.map( _._2 ) )
      case Access( e1, e2 ) => fv( e1 ) ++ fv( e2 )
      case MyList(es) => fvSeq(es)
      case Head(e) => fv(e)
      case Tail(e) => fv(e)
    }
  }

  // evaluate a sequence of expressions returning a list of the
  // corresponding values.  It is up to the caller to pop extra
  // things off of the root set
  def evalS( ts:Seq[Term] ): Seq[Storable] = {
    ts.map(t => {
      val v = eval(t)
      RootSet.pushExtra(v)
      v
    })
  }

  // the main evaluation function
  def eval( t:Term ): Storable = t match {
    case Then( ts ) ⇒ 
      ts.foldLeft(UndefV(): Storable)((_, cur) =>
	eval(cur))
    
    // notice that e is evaluated _before_ we check whether x is legal
    case Assign( Var(x), e ) ⇒ 
    {
      val v = eval( e )

      σ( ρ( x ) ) = v
    }
    
    case w @ While( e, t ) ⇒ 
    {
      val v = eval( e )

      if ( v.T ) {
	eval( t )
	eval( w )
      }
      else UndefV()
    }

    case Output( e ) ⇒ 
    {
      val v = eval( e )

      println( v )
      UndefV()
    }
    
    case Update( e1, e2, e3 ) ⇒
    {
      val adr = eval( e1 )
      RootSet.pushExtra(adr)
      val fld = eval( e2 )
      RootSet.pushExtra(fld)
      val rhs = eval( e3 )
      RootSet.pushExtra(rhs)

      (adr, fld) match {
	case (adr:Address, fld:StrV) ⇒ 
	  {
	    σ(adr) = toObj( σ(adr) ) :+ (fld → rhs)
	    RootSet.popExtraTimes(3)
	    UndefV()
	  }
	case _ ⇒ throw undefined
      }
    }

    case Num( n ) ⇒ 
      n

    case Bool( b ) ⇒ 
      b
      
    case Str( s ) ⇒ 
      s
      
    case Undef() ⇒ 
      UndefV()

    case Var( x ) ⇒ 
      σ( ρ( x ) )

    case UnOp( op, e ) ⇒ 
    {
      val v = eval( e )

      op match {
	case ⌞−⌟ ⇒ v neg
	case ⌞¬⌟ ⇒ v not
      }
    }
    
    case BinOp( op, e1, e2 ) ⇒ 
    {
      val v1 = eval( e1 )
      RootSet.pushExtra(v1)
      val v2 = eval( e2 )

      val retval = op match {
	case ⌜+⌝ ⇒ v1 + v2
	case ⌜−⌝ ⇒ v1 − v2
	case ⌜×⌝ ⇒ v1 × v2
	case ⌜÷⌝ ⇒ v1 ÷ v2
	case ⌜∧⌝ ⇒ v1 ∧ v2
	case ⌜∨⌝ ⇒ v1 ∨ v2
	case ⌜=⌝ ⇒ v1 ≈ v2
	case ⌜≠⌝ ⇒ v1 ≠ v2
	case ⌜≤⌝ ⇒ v1 ≤ v2
	case ⌜<⌝ ⇒ v1 < v2
	case ⌜::⌝ ⇒ v2 match {
	  case a: Address =>
	    ListV.cons(v1, a)
	  case _ => throw undefined
	}
      }
      RootSet.popExtra()
      retval
    }
    
    case If( e, t1, t2 ) ⇒ 
    {
      val v = eval( e )
      RootSet.pushExtra(v)
      val retval = if ( v.T ) eval( t1 ) else eval( t2 )
      RootSet.popExtra()
      retval
    }
    
    case In( typ ) ⇒ 
      typ match {
	case NumIT ⇒ BigInt( Console.readLine() )
	case StrIT ⇒ Console.readLine()
      }
    
    case Let( xs, t ) ⇒ 
    {
      val addrs = addBindings(xs.map(_.x → UndefV()))
      val retval = eval(t)
      ρ.popTimes(addrs.size)
      retval
    }

    case fun@Fun( xs, t ) ⇒ 
      CloV(xs map (_.x), t, closureEnv(fun))
    
    case Call( ef, es ) ⇒ 
    {
      val fun = eval( ef )
      RootSet.pushExtra(fun)
      val args = ρ( "gObj" ) +: evalS( es )
      val retval = applyClo( fun, args )
      RootSet.popExtraTimes(es.length + 1)
      retval
    }

    case MCall( e1, e2, es ) ⇒
    {
      val adr = eval( e1 )
      RootSet.pushExtra(adr)
      val fld = eval( e2 )
      RootSet.pushExtra(fld)
      //println("OTHER AST NODE: " + t)
      val method = (adr, fld) match {
	case (adr:Address, fld:StrV) ⇒ toObj( σ(adr) )( fld )
	case _ ⇒ throw undefined
      }
      val args = evalS( es )
      val retval = applyClo( method, adr +: args )
      RootSet.popExtraTimes(es.length + 2)
      retval
    }

    case Obj( fbs ) ⇒
    {
      val (xs, es) = fbs unzip; // semicolon required

      // fields and values
      val fs = xs.map(s => StrV(s.s))
      val vs = evalS(es)
      val retvalObj = fs.zip(vs).foldLeft(ObjectV())(
	(res, cur) =>
	  res :+ cur)
      RootSet.pushExtra(retvalObj)
      val retval = (σ += retvalObj)
      RootSet.popExtraTimes(vs.length + 1)
      retval
    }

    case Access( e1, e2 ) ⇒
    {
      val adr = eval( e1 )
      RootSet.pushExtra(adr)
      val fld = eval( e2 )

      val retval = (adr, fld) match {
	case (adr:Address, fld:StrV) ⇒ toObj( σ(adr) )( fld )
	case _ ⇒ throw undefined
      }
      RootSet.popExtra()
      retval
    }

    case MyList( es ) ⇒ {
      val retval = evalS(es).foldRight(σ.listNilAddress)(
	(cur, res) => 
	  ListV.cons(cur, res))
      RootSet.popExtraTimes(es.length)
      retval
    }

    case Head( e ) ⇒
      eval( e ) match {
	case a: Address =>
	  σ(a) match {
	    case Cell( hd, _ ) ⇒ hd
	    case _ ⇒ throw undefined
	  }
	case _ => throw undefined
      }

    case Tail( e ) ⇒
      eval( e ) match {
	case a: Address =>
	  σ(a) match {
	    case Cell( _, tl ) ⇒ tl
	    case _ ⇒ throw undefined
	  }
	case _ => throw undefined
      }
  }

  // apply a closure
  def applyClo( fun:Storable, args:Seq[Storable] ): Storable =
    fun match { 
      case CloV(xs, t, _ρ) ⇒ 
      {
	if (xs.length != args.length) throw undefined
	else {
	  ρ ++ _ρ.env.toSeq
	  val addrs = addBindings(xs.zip(args))
	  val retval = eval(t)
	  ρ.popTimes(addrs.size)
	  ρ.popTimes(_ρ.env.size)
	  retval
	}
      }
      case _ ⇒ throw undefined
    }
}
