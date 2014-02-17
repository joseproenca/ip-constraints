package reopp.common.benchmarks

import reopp.workers.Deployer
import reopp.common.guardedcommands.dataconnectors.ConnectorGen._
import reopp.common.guardedcommands.GCConnector
import reopp.common.Function
import reopp.common.Predicate
import scala.util.Random
import reopp.common.Solution
import reopp.common.OptionSol
import reopp.common.IntFunction
import choco.kernel.model.variables.integer.IntegerExpressionVariable
import choco.Choco
import z3.scala.Z3Context
import z3.scala.Z3AST
import reopp.common.IntPredicate
import reopp.common.choco.ChoConnector
import reopp.common.choco.dataconnectors._
import reopp.workers.Node
import reopp.common.guardedcommands.Formula
import reopp.common.guardedcommands.GCSolution
import scala.actors.Actor._
import scala.actors.Actor
import reopp.common.NoneSol
import reopp.common.guardedcommands.dataconnectors.GCReader
import reopp.workers.strategies.GenEngine

/**
 * @author Jose Proenca
 * 
 * Benchmarks used for the submitted journal paper - Special Issue for FOCLASA'13.
 * Experimenting Interactive Partial Constraints - tests measure the time to solve
 * the dynamic Choco constraints [[reopp.common.guardedcommands.chocox]],
 * using 1 or more workers [[reopp.common.workers]].
 * 
 * Usage: SCP14 <connector> <size> <method>
 * Possible connectors: "sensors", "transitions-spar", "transitions-sseq",... 
 *
 */
object SCP14 {
  
  def printHelp {
      System.err.println(
"""Wrong usage. It should be: SCP14 <connector> <size> <method>
Possible connectors: "sensors", "transactions-spar", "transactions-sseq1",
  "transactions-sseqn", "transactions-bpar", "transactions-bseq1",
  "transactions-bseqn", "pairwise"
Possible methods: "sat", "smt", "all", "partial1", "partial2", "partial4" 
""")
  }
  
  def main(args:Array[String]) {
  
  if (args.size < 3) {
    printHelp
    return
  }
  
  Warmup.go

  
//  args(0)="pairwise"
//  args(1)="20"
//  args(2)="all"
//  args(3)=""
  
  val n = Integer.parseInt(args(1))
  val sat = args(2) startsWith "sat"
  val smt = args(2) startsWith "smt"
  val ahc = args(2) startsWith "ahc"
  val all = args(2) startsWith "all"
  val p1 = args(2) startsWith "partial1"
  val p2 = args(2) startsWith "partial2"
  val p4 = args(2) startsWith "partial4"
  val debug = (args.size > 3) && (args(3) startsWith "debug")

  
  def show(time:Long,mode:String,res:OptionSol[Solution]) {
      if (debug) { 
        if (res.isDefined) println(mode+" - solved in "+time+" ms:\n"+res.get)
        else println(mode+" - no solution (in "+time+" ms)")
      }
      else print(time+" ") //println(mode+"        - "+time)            
  }
  
  //////////////////////////
  /// Sensors experiment ///
  //////////////////////////

  if (args(0) startsWith "sensors") {
    if (debug) println("Sensors")
    
    case class TimedTemp(temp:Int,unit:String
                    ,hour:Int,min:Int)    
                    
    val rand = new Random(0); // seed makes it deterministic.                    

    def genTimedTemp =
      TimedTemp((25*rand.nextFloat).toInt
    		   ,if(rand.nextBoolean) "C" else "F"
    		   ,(24*rand.nextFloat).toInt
    		   ,(60*rand.nextFloat).toInt) 

    def genWriters(i:Int): GCConnector = i match {
      case 1 => writer("w1",List(genTimedTemp))
      case i if i>0 => writer("w"+i,List(genTimedTemp)) ++ genWriters(i-1)
    }
    
    def getTime = Function("getTime"){
    	case TimedTemp(_,_,h,m) => (h,m)
    }
    def getTemp = Function("getTemp"){
    	case TimedTemp(t,u,_,_) => (t,u)
    }
	def f2c = Function("F2C"){
	  case (t:Int,_) => ((t-32) * 5/9,"C")
	}
	def night = Predicate("night"){
	  case (h:Int,m:Int) =>
//	    println(s"is $h:$m night? - "+(h+60*m > 1200 || h+60*m < 420))
	    h+60*m > 1200 ||
	    h+60*m < 420
	}
	def isF = Predicate("isF"){
	  case (_,u) => u == "F"
	}
	def isC = Predicate("isC"){
	  case (_,u) => u == "C"
	}
    
    val conn =
      genWriters(n) ++
      nmerger((for(i<-1 to n) yield "w"+i).toList, "w") ++
      transf("w","gTime",getTime)++
      transf("w","gTemp",getTemp)++
      negfilter("gTime","night",night) ++
      filter("gTemp","isF",isF)++
      transf("isF","f2c",f2c) ++
      filter("gTemp","isC",isC)++
      merger("isC","f2c","ctemp") ++
      sdrain("night","ctemp")++
      reader("ctemp")

      
    // Running the experiments //
          
    if (sat) {
      val cons = conn.getConstraints
      cons.close
      val time = System.currentTimeMillis()
      val res = cons.solveChocoSat
      val spent = System.currentTimeMillis() - time
      show(spent,"SAT",res)
    }
    else if (smt) {
      val cons = conn.getConstraints
      val time = System.currentTimeMillis()
      val res = cons.solveChocoX
      val spent = System.currentTimeMillis() - time
      show(spent,"SMT",res)
    }
    else {
      printHelp
      return
    }
    /*
    else if (ahc) {
		  // convert to min + hour*60 + temp*60*24 + temp*60*24*2
	    def genTimedTemp2 =
		    (25*rand.nextFloat).toInt *60*24*2 +
		    (if(rand.nextBoolean) 0 else 1) *60*24 +
		    (24*rand.nextFloat).toInt *60 +
		    (60*rand.nextFloat).toInt 

		def genWriters2(i:Int): ChoConnector = i match {
		  case 1 => new ChoWriter("w1",0,List(genTimedTemp2))
		  case i if i>0 => new ChoWriter("w"+i,0,List(genTimedTemp2)) ++ genWriters2(i-1)
		}
		val getTimeCho = (x:IntegerExpressionVariable) =>
		      // recover hours and min
//		      Choco.mod(x,60*24)
		  	  Choco.plus(x,2) // MODULO IS BROKEN!
		val getTempCho = (x:IntegerExpressionVariable) =>
		      // recover temp and unit - (x-x%(60*24))/(60*24)
//		      Choco.div(Choco.minus(x,getTimeCho(x)),60*24)
		  	  Choco.plus(x,10)
		val f2cCho = (x:IntegerExpressionVariable) =>
		      // convert temp (and unit) to celcius
		      // ((x-(x%2)-32 * 5/9) + 0) - zero is the code for Celcius
//		      Choco.div(Choco.mult(Choco.minus(Choco.minus(x,Choco.mod(x,2)),32),5),9)
//		      Choco.div(Choco.mult(Choco.minus(x,32),5),9)
		  	  Choco.plus(x,100)
		val notNightCho = (x:IntegerExpressionVariable) =>
			  // x>1200 || x<420
//			  Choco.not(Choco.or(Choco.gt(x,1200),Choco.lt(x,420)))
		  	  Choco.TRUE
		val isFCho = (x:IntegerExpressionVariable) =>
			  // x%2 = 1
//			  Choco.eq(Choco.mod(x,2),1)
			  Choco.TRUE
		val isCCho = (x:IntegerExpressionVariable) =>
			  // x%2 = 0
//			  Choco.eq(Choco.mod(x,2),0)
			  Choco.FALSE
        val conn2 =
	      genWriters2(n) ++
	      new ChoNMerger((for(i<-1 to n) yield "w"+i).toList, "w",0) ++
//	      new ChoTransf("w","gTime",0,(x:IntegerExpressionVariable) => Choco.mod(x,2))//++
	      new ChoTransf("w","gTime",0,getTimeCho)++
	      new ChoTransf("w","gTemp",0,getTempCho)++
	      new ChoFilter("gTime","night",0,notNightCho) ++
	      new ChoFilter("gTemp","isF",0,isFCho)++
	      new ChoTransf("isF","f2c",0,f2cCho) ++
	      new ChoFilter("gTemp","isC",0,isCCho)++
	      new ChoMerger("isC","f2c","ctemp",0) ++
	      new ChoSDrain("night","ctemp",0)++
	      new ChoReader("ctemp",0,1)

		val cons = conn2.getConstraints
//		println(cons)
		cons.close
		val time = System.currentTimeMillis()
        val res = cons.solve()
        val spent = System.currentTimeMillis() - time
        show(spent,"AHC",res)
    }
    */
    
    
  }
  
  ///////////////////////////
  /// Transactions - Sync ///
  ///////////////////////////

  else if (args(0) startsWith "transactions-s") {
    val seq = args(0) startsWith "transactions-sseq" 
    val fst = args(0) startsWith "transactions-sseq1" 
    if (debug) {
      if (fst)      println("transactions-sseq-failFst")
      else if (seq) println("transactions-sseq-failLast")
      else	      println("transactions-spar")
    }
    
    val id = Function("id") {
      case x => x
    }
    val fail = Predicate("fail") {
      case _ => false // { print("fail-"); false}
    }
    val success = Predicate("success") {
      case _ => true // { print("succ-"); true }
    }
    def genS(i:Int,si:Int,o:Int,f:Function,ok:Predicate,fi:Function) = {
//      println(s"generating S from $i to $o")
      transf("out"+i,"a"+o,f) ++
      filter("a"+o,"out"+o,ok) ++
      negfilter("a"+o,"b"+o,ok) ++
      merger("b"+o,"sto"+o,"c"+o) ++
      transf("c"+o,"sto"+si,fi)      
    }
    def genSSeqs(from:Int,to:Int): GCConnector = to match {
      case 0 => empty()
      case `from` => genS(to,to,to+1,id,success,id)
      case _ if to>0 => genS(to,to,to+1,id,success,id) ++ genSSeqs(from,to-1)
    }
    def genSPars(i:Int): GCConnector = i match {
      case 0 => empty()
      case 1 => genS(1,-2,2,id,success,id)
      case _ if i>0 => genS(i,-i-1,i+1,id,success,id) ++ genSPars(i-1)
    }
    def genNoflow(i:Int): GCConnector = i match {
      case 0 => empty()
      case 1 => noflow("sto2")
      case _ if i>0 => noflow("sto"+(i+1)) ++ genNoflow(i-1)
    }
    
	val conn = if (fst)
	  writer("out1",List(1)) ++
      reader("sto1",1) ++
      genS(1,1,2,id,fail,id) ++
      genSSeqs(2,n) ++
      noflow("sto"+(n+1))
    else if (seq)        
	  writer("out1",List(1)) ++
      reader("sto1",1) ++
      genSSeqs(1,n-1) ++
      genS(n,n,n+1,id,fail,id) ++
      noflow("sto"+(n+1))
	else
      writer("out1",List(1)) ++
      reader("sto1",1) ++
      genSPars(n) ++
      genNoflow(n)
      
      
    // Running the experiments //
          
    if (sat) {
      val cons = conn.getConstraints
      cons.close
      val time = System.currentTimeMillis()
      val res = cons.solveChocoSat
      val spent = System.currentTimeMillis() - time
      show(spent,"SAT",res)
    }
    else if (smt) {
      val cons = conn.getConstraints
      val time = System.currentTimeMillis()
      val res = cons.solveChocoDyn
      val spent = System.currentTimeMillis() - time
      show(spent,"SMT",res)
    } 
    else {
      printHelp
      return
    }
  }

  ///////////////////////////////
  /// Transactions - Built-in ///
  ///////////////////////////////

  else if (args(0) startsWith "transactions-b") {
    val seq = args(0) startsWith "transactions-bseq" 
    val fst = args(0) startsWith "transactions-bseq1" 
    if (debug) {
      if (fst)      println("transactions-bseq-failFst")
      else if (seq) println("transactions-bseq-failLast")
      else	      println("transactions-bpar")
    }
 
    val id = Function("id") {
      case x => x
    }
    val fail = Predicate("fail") {
      case _ => false //{ print("fail-"); false}
    }
    val success = Predicate("success") {
      case _ => true //{ print("succ-"); true }
    }
    def genB(i:Int,o:Int,f:Function,ok:Predicate,fi:Function) = {
      transf("out"+i,"a"+o,f,fi) ++
      filter("a"+o,"out"+o,ok) ++
      sdrain("a"+o,"out"+o)
    }
    def genBSeqs(from:Int,to:Int): GCConnector = to match {
      case 0 => empty()
      case `from` => genB(to,to+1,id,success,id)
      case _ if to>0 => genB(to,to+1,id,success,id) ++ genBSeqs(from,to-1)
    }
    def genBPars(i:Int): GCConnector = i match {
      case 0 => empty()
      case 1 => genB(1,2,id,success,id)
      case _ if i>0 => genB(i,i+1,id,success,id) ++ genBPars(i-1)
    }
    
    val conn = if (fst)
      writer("out1",List(1)) ++
      genB(1,2,id,fail,id) ++
      genBSeqs(2,n)
    else if (seq)        
	  writer("out1",List(1)) ++
      genBSeqs(1,n-1) ++
      genB(n,n+1,id,fail,id)
    else
      writer("out1",List(1)) ++
      genBPars(n)
      
    // Running the experiments //
          
    if (sat) {
      val cons = conn.getConstraints
      cons.close
      val time = System.currentTimeMillis()
      val res = cons.solveChocoSat
      val spent = System.currentTimeMillis() - time
      show(spent,"SAT",res)
    }
    else if (smt) {
      val cons = conn.getConstraints
      val time = System.currentTimeMillis()
      val res = cons.solveChocoDyn
      val spent = System.currentTimeMillis() - time
      show(spent,"SMT",res)
    } 
    else {
      printHelp
      return
    }
  }
  
  /////////////////////////////
  /// Parallel asyncrhonous ///
  /////////////////////////////

  else if (args(0) startsWith "pairwise") {
    if (debug) println("Pairwise asynchronous")
	
    val workers: Int = if (p1) 1 else if (p2) 2 else if (p4) 4 else 0 
//    				   else throw new RuntimeException("Only 1, 2, or 4 workers.")
    if (!(p1||p2||p4||all)) {
      System.err.println("Only 1, 2, or 4 workers, or \"all\".")
      printHelp
      return
    }
    
	val engine = if (all) GenEngine.all(1)
				   else     GenEngine.hybrid(workers)
    

	val isEven = Predicate("isEven") {
	  case i:Int => i%2 == 0
	}
	def genPair(i:Int) = engine add (
      writer("x"+i,
          List(i)) ++ 
          //for (n<-(1 to 50).toList) yield i) ++ 
      filter("x"+i,"y"+i,isEven) //++
//      reader("y"+i)
//      genReader(i) 
    ,priority = Set("x"+i)
    )
    def connectPair(i:Int,n1:Node[GCSolution,Formula],n2:Node[GCSolution,Formula]) = {
      val ad = engine add (adrain("x"+i,"x"+(i+1)))
      ad("x"+i)  <-- n1("x"+i)
      ad("x"+(i+1))  <-- n2("x"+(i+1))
      ad
    } 
    
    var prev = genPair(1) // n >= 1
    for (i <- 2 to n) {
      val next = genPair(i)
      connectPair(i-1,prev,next)
      prev = next
    }
    
        // Running the experiments //

//    counter.time = System.currentTimeMillis()
//    counter.start
    
    val time = System.currentTimeMillis()
    engine.init
    engine.awaitTermination
    val spent = System.currentTimeMillis() - time
    show(spent,if (all) "ALL" else "Partial"+workers,NoneSol())
    
  }
}}