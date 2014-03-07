package reopp.common.benchmarks

import reopp.common.guardedcommands.dataconnectors.ConnectorGen._
import reopp.common.guardedcommands.GCConnector
import reopp.common.Function
import reopp.common.Predicate
import scala.util.Random
import reopp.common.Solution
import reopp.common.OptionSol
import reopp.common.IntFunction
import choco.kernel.model.variables.integer.IntegerExpressionVariable
import reopp.common.IntPredicate
import reopp.common.choco.ChoConnector
import reopp.common.choco.dataconnectors._
import reopp.workers.Node
import reopp.common.guardedcommands.Formula
import reopp.common.guardedcommands.GCSolution
import reopp.common.NoneSol
import reopp.common.guardedcommands.dataconnectors.GCReader
import reopp.workers.strategies.GenEngine
import reopp.common.guardedcommands.dataconnectors.GCWriter
import reopp.common.guardedcommands.True

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
Possible connectors: "sensors", "sensortree", "sensortreetriv",  "transactions-spar",
  "transactions-sseq1", "transactions-sseqn", "transactions-bpar",
  "transactions-bseq1", "transactions-bseqn", "pairwise"
Possible methods: "sat", "smt", "all", "partial"<n>, "onebyone"<n>, "all0"
""")
  }
  
  def main(args:Array[String]) {
  
  if (args.size < 3) {
    printHelp
    return
  }
  
  Warmup.go

  
//  args(0)="sensortree"
//  args(1)="1"
//  args(2)="partial0"
//  args(3)=""
  
  val n = Integer.parseInt(args(1))
  val sat = args(2) startsWith "sat"
  val smt = args(2) startsWith "smt"
  val ahc = args(2) startsWith "ahc"
  val all = args(2) startsWith "all"
  val part=args(2) startsWith "partial"
  val obo =args(2) startsWith "onebyone"
//  val pvt = args(2) endsWith "0"

  val nat = "[a-zA-Z]".r.replaceAllIn(args(2),"")
  val pvt = nat == "0"    
  val workers = if (!pvt && (nat matches "[0-9]+")) nat.toInt else 1

  val debug = (args.size > 3) && (args(3) startsWith "debug")

//  val workers: Int = if (t1) 1 else if (t2) 2 else if (t4) 4 else 1 
//  if ((part||obo)&&(!(t1||t2||t4||pvt))) {
//      System.err.println("Only 1, 2, or 4 workers, or \"all\".")
//      printHelp
//      return
//  }
    
  val engine = if (all) GenEngine.all(1)
               else if (part) GenEngine.hybrid(workers)
               else if (obo)  GenEngine.oneStep(workers)
               else GenEngine.all(1) // declared, but only constraints are used.
               
  def wiredWriter(sk:String,dt:Any) =
    new GCWriter(sk,0,List(dt)) {
              override def update(s:OptionSol[S]) {
                if (pvt) {
//                  println("killing engine earlier! - sol:\n"+s)
                  engine.kill
                }
                else super.update(s)
              }
            }

  type S = GCSolution
  type C = Formula
  
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
    engine.kill
    
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
      transf("w","gTemp",getTemp) ++
      negfilter("gTime","night",night) ++
      filter("gTemp","isF",isF)++
      transf("isF","f2c",f2c) ++
      filter("gTemp","isC",isC)++
      merger("isC","f2c","ctemp") ++
      sdrain("night","ctemp")++
      reader("ctemp")

//      genWriters(n) ++
//      nmerger((for(i<-1 to n) yield "w"+i).toList, "w") ++
//      transf("w","gTime",getTime)++
//      transf("w","gTemp",getTemp)++
//      negfilter("gTime","night",night) ++
//      filter("gTemp","isF",isF)++
//      transf("isF","f2c",f2c) ++
//      filter("gTemp","isC",isC)++
//      merger("isC","f2c","ctemp") ++
//      sdrain("night","ctemp")++
//      reader("ctemp")
      
    // Running the experiments //
          
    if (sat) {
      val cons = conn.getConstraints.close
//      cons.close
      val time = System.currentTimeMillis()
      val res = cons.solveChocoSat
      val spent = System.currentTimeMillis() - time
      show(spent,"SAT",res)
    }
    else if (smt) {
      val cons = conn.getConstraints.close
//      println(cons)
      val time = System.currentTimeMillis()
      val res = cons.solveChocoDyn
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
  
  
  //////////////////////////////
  /// Sensor Tree experiment ///
  //////////////////////////////

  if (args(0) startsWith "sensortree") {
    if (debug) println("Tree of Sensors")
    
    // if "triv" is part of the name, then night predicate always fails,
    // to make sure there is always dataflow.
    val forceNight = args(0) startsWith "sensortreetriv"
    
    
    case class TimedTemp(temp:Int,unit:String
                    ,hour:Int,min:Int)    
                    
    val rand = new Random(0); // seed makes it deterministic.                    

    def genTimedTemp =
      TimedTemp((25*rand.nextFloat).toInt
    		   ,if(rand.nextBoolean) "C" else "F"
    		   ,(24*rand.nextFloat).toInt
    		   ,(60*rand.nextFloat).toInt) 
    
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
	    // night means between 20h and 7h.
	    !forceNight &&
	    (h+60*m > 1200 ||
	     h+60*m < 420)
	}
	def isF = Predicate("isF"){
	  case (_,u) => u == "F"
	}
	def isC = Predicate("isC"){
	  case (_,u) => u == "C"
	}
	
    
	// build the core node of the connector
	val mainCon = 
	//      genWriters(n) ++
//      nmerger((for(i<-1 to n) yield "w"+i).toList, "w") ++
      transf("x","gTime",getTime)++
      transf("x","gTemp",getTemp)++
      negfilter("gTime","night",night) ++
      filter("gTemp","isF",isF)++
      transf("isF","f2c",f2c) ++
      filter("gTemp","isC",isC)++
      merger("isC","f2c","ctemp") ++
      sdrain("night","ctemp")//++
//      new GCReader("ctemp",0,n) {
//        override def isProactive = false // passive reader
//      }
      //reader("ctemp")

    // keep track of the constraints for SAT vs SMT.
    // ALWAYS add here before adding to the engine - will change variable names to be unique.
    var const = mainCon.getConstraints
    
    val mainNode = engine add mainCon


    
    // make the generators of the mergers and writers
//	val height = (Math.log(n)/Math.log(2)).toInt - 1
    val height = n
	
	def addMrgLevel(sks:Iterable[(Node[S,C],String)]): Iterable[(Node[S,C],String)] = {
      val res = scala.collection.mutable.Set[(Node[S,C],String)]()
      for ((nd,sk) <- sks) {
	    val newc = merger(sk+"0",sk+"1",sk)
	    const ++= newc.getConstraints
	    val newNd = engine add (
	        newc,
	        deps = Set(sk+"0"->sk,sk+"1"->sk)
	    )
	    nd(sk) <-- newNd(sk)
	    res += ((newNd,sk+"0"))
	    res += ((newNd,sk+"1"))
      }
      res
    } 
    def addWriters(sks:Iterable[(Node[S,C],String)]) {
      for ((nd,sk) <- sks) {
        val wrC = wiredWriter(sk,genTimedTemp)
        const ++= wrC.getConstraints
        val wrNd = engine.add(
            wrC,
            priority = Set(sk))
        nd(sk) <-- wrNd(sk)
      }
    }
	
    // use the generators to produce all the nodes
    var mrg = Set[(Node[S,C],String)]((mainNode -> "x")).toIterable
	for (h <- 0 to height) {
	  mrg = addMrgLevel(mrg)
	}
    addWriters(mrg)

      
    // Running the experiments //    
    
    if (sat) {
      engine.kill
      val cons = const.close
      val time = System.currentTimeMillis()
      val res = cons.solveChocoSat
      val spent = System.currentTimeMillis() - time
      show(spent,"SAT",res)
    }
    else if (smt) {
      engine.kill
      val cons = const.close
      val time = System.currentTimeMillis()
      val res = cons.solveChocoDyn
      val spent = System.currentTimeMillis() - time
      show(spent,"SMT",res)
    }
    else if (all||part||obo) {
      val time = System.currentTimeMillis()
	  engine.init
	  engine.awaitTermination
	  val spent = System.currentTimeMillis() - time
	  show(spent,
	      (if (all) "ALL" else if (part) "Partial"+workers else "OneByOne"+workers)+
	      (if (pvt) "-0" else ""),NoneSol())
    }
    else {
      engine.kill
      printHelp
      return
    }
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
      case _ =>
        false
//        { print("fail-"); false}
    }
    val success = Predicate("success") {
      case _ => 
        true
//        { print("succ-"); true }
    }

    // defining the nodes and constraints
    val wrCon = wiredWriter("out1",1)
    var const = wrCon.getConstraints
    val starting = engine add wrCon    	   

    def genS(i:Int,si:Int,o:Int,f:Function,ok:Predicate,fi:Function)
    		: (Node[S,C],String,String,String,String) = {
//      println(s"generating S from $i to $o")
      val newS =
	      transf("out"+i,"a"+o,f) ++
	      filter("a"+o,"out"+o,ok) ++
	      negfilter("a"+o,"b"+o,ok) ++
	      merger("b"+o,"sto"+o,"c"+o) ++
	      transf("c"+o,"sto"+si,fi)
      const ++= newS.getConstraints
      val nd = engine add newS
      (nd,"out"+i,"out"+o,"sto"+o,"sto"+si)
    }
    def genSSeqs(from:Int,to:Int): (Node[S,C],String,String,Node[S,C],String,String) = to match {
      case 0 => throw new Exception("zero sequence not allowed")
      case `from` =>
        val (n,a,b,c,d) = genS(to,to,to+1,id,success,id)
        (n,a,d,n,b,c)
      case _ if to>0 =>
        val (n2,e,f,n3,g,h) = genSSeqs(from,to-1)
        val (n,a,b,c,d) = genS(to,to,to+1,id,success,id)
        n(a) <-- n3(g)
        n3(h) <-- n(d)
        (n2,e,f,n,b,c)
//        genS(to,to,to+1,id,success,id) ++ genSSeqs(from,to-1)
    }
//    def genSPars(i:Int): Connector[S,C] = i match {
//      case 0 => empty()
//      case 1 => genS(1,-2,2,id,success,id)
//      case _ if i>0 => genS(i,-i-1,i+1,id,success,id) ++ genSPars(i-1)
//    }
//    def genNoflow(i:Int): Connector[S,C] = i match {
//      case 0 => empty()
//      case 1 => noflow("sto2")
//      case _ if i>0 => noflow("sto"+(i+1)) ++ genNoflow(i-1)
//    }
        
    if (n==1) {
      val (nd,a,b,c,d) = genS(1,1,2,id,fail,id)
      val nfCon = noflow("sto2")
      const ++= nfCon.getConstraints
      val nf = engine add nfCon
      nd(a) <-- starting("out1")
      nd(c) <-- nf("sto"+(n+1))

    }
    else if (fst) {
      val (nd,a,b,c,d) = genS(1,1,2,id,fail,id)
      val (n2,e,f,n3,g,h) = genSSeqs(2,n)
      val nfCon = noflow("sto"+(n+1))
      const ++= nfCon.getConstraints
      val nf = engine add nfCon
      nd(a) <-- starting("out1")
      n2(e) <-- nd(b)
      nd(c) <-- n2(f)
      n3(h) <-- nf("sto"+(n+1))
    }
    else if (seq) {
      val (n2,e,f,n3,g,h) = genSSeqs(1,n-1)
      val (nd,a,b,c,d) = genS(n,n,n+1,id,fail,id)
      val nfCon = noflow("sto"+(n+1))
      const ++= nfCon.getConstraints
      val nf = engine add nfCon
      n2(e) <-- starting("out1")
      nd(a) <-- n3(g)
      n3(h) <-- nd(d)
      nd(c) <-- nf("sto"+(n+1))
    }
    else
      throw new Exception("Parallel transactions not done yet.")

    
//    println("+----\n"+engine.pretty+"\n+----")

    
    
//	val conn = if (fst)
//	  writer("out1",List(1)) ++
//      reader("sto1",1) ++
//      genS(1,1,2,id,fail,id) ++
//      genSSeqs(2,n) ++
//      noflow("sto"+(n+1))
//    else if (seq)        
//	  writer("out1",List(1)) ++
//      reader("sto1",1) ++
//      genSSeqs(1,n-1) ++
//      genS(n,n,n+1,id,fail,id) ++
//      noflow("sto"+(n+1))
//	else
//      writer("out1",List(1)) ++
//      reader("sto1",1) ++
//      genSPars(n) ++
//      genNoflow(n)
      
      
    // Running the experiments //
          
    if (sat) {
//      val cons = conn.getConstraints.close
      val cons = const.close
      val time = System.currentTimeMillis()
      val res = cons.solveChocoSat
      val spent = System.currentTimeMillis() - time
      engine.kill
      show(spent,"SAT",res)
    }
    else if (smt) {
//      val cons = conn.getConstraints.close
      val cons = const.close
//      println(const)
      val time = System.currentTimeMillis()
      val res = cons.solveChocoDyn
      val spent = System.currentTimeMillis() - time
      engine.kill
      show(spent,"SMT",res)
    } 
    else if (all||part||obo) {
      val time = System.currentTimeMillis()
	  engine.init
	  engine.awaitTermination
	  val spent = System.currentTimeMillis() - time
	  show(spent,
	      (if (all) "ALL" else if (part) "Partial"+workers else "OneByOne"+workers)+
	      (if (pvt) "-0" else ""),NoneSol())
    }
    else {
      engine.kill
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
      case _ =>
        false
//        { print("fail-"); false}
    }
    val success = Predicate("success") {
      case _ => 
        true
//        { print("succ-"); true }
    }

    // defining the nodes and constraints
    val wrCon = wiredWriter("out1",1)
    var const = wrCon.getConstraints
    val starting = engine add wrCon    	   

    def genB(i:Int,o:Int,f:Function,ok:Predicate,fi:Function)
    		: (Node[S,C],String,String) = {
//      println(s"generating S from $i to $o")
      val conn =
          transf("out"+i,"a"+o,f,fi) ++
          filter("a"+o,"out"+o,ok) ++
          sdrain("a"+o,"out"+o)
      const ++= conn.getConstraints
	  val  nd = engine add conn
      (nd,"out"+i,"out"+o)
    }
    def genBSeqs(from:Int,to:Int): (Node[S,C],String,Node[S,C],String) = to match {
      case 0 => throw new Exception("zero sequence not allowed")
      case `from` =>
        val (n,a,b) = genB(to,to+1,id,success,id)
        (n,a,n,b)
      case _ if to>0 =>
        val (n2,e,n3,g) = genBSeqs(from,to-1)
        val (n,a,b) = genB(to,to+1,id,success,id)
        n(a) <-- n3(g)
        (n2,e,n,b)
    }
        
    if (n==1) {
      val (nd,a,b) = genB(1,2,id,fail,id)
      nd(a) <-- starting("out1")
    }
    else if (fst) {
      val (nd,a,b) = genB(1,2,id,fail,id)
      val (n2,e,n3,g) = genBSeqs(2,n)
      nd(a) <-- starting("out1")
      n2(e) <-- nd(b)
    }
    else if (seq) {
      val (n2,e,n3,g) = genBSeqs(1,n-1)
      val (nd,a,b) = genB(n,n+1,id,fail,id)
      n2(e) <-- starting("out1")
      nd(a) <-- n3(g)
    }
    else
      throw new Exception("Parallel transactions not done yet.")

    
//    println("+----\n"+engine.pretty+"\n+----")

    // Running the experiments //
          
    if (sat) {
//      val cons = conn.getConstraints.close
      val cons = const.close
      val time = System.currentTimeMillis()
      val res = cons.solveChocoSat
      val spent = System.currentTimeMillis() - time
      engine.kill
      show(spent,"SAT",res)
    }
    else if (smt) {
//      val cons = conn.getConstraints.close
      val cons = const.close
      val time = System.currentTimeMillis()
      val res = cons.solveChocoDyn
      val spent = System.currentTimeMillis() - time
      engine.kill
      show(spent,"SMT",res)
    } 
    else if (all||part||obo) {
      val time = System.currentTimeMillis()
	  engine.init
	  engine.awaitTermination
	  val spent = System.currentTimeMillis() - time
	  show(spent,
	      (if (all) "ALL" else if (part) "Partial"+workers else "OneByOne"+workers)+
	      (if (pvt) "-0" else ""),NoneSol())
    }
    else {
      engine.kill
      printHelp
      return
    }
  }
  
//  //////////////////////////////////////////
//  /// Transactions - Built-in - no nodes ///
//  //////////////////////////////////////////
//
//  else if (args(0) startsWith "transactions-b") {
//    val seq = args(0) startsWith "transactions-bseq" 
//    val fst = args(0) startsWith "transactions-bseq1" 
//    if (debug) {
//      if (fst)      println("transactions-bseq-failFst")
//      else if (seq) println("transactions-bseq-failLast")
//      else	      println("transactions-bpar")
//    }
// 
//    val id = Function("id") {
//      case x => x
//    }
//    val fail = Predicate("fail") {
//      case _ => false //{ print("fail-"); false}
//    }
//    val success = Predicate("success") {
//      case _ => true //{ print("succ-"); true }
//    }
//    def genB(i:Int,o:Int,f:Function,ok:Predicate,fi:Function) = {
//      transf("out"+i,"a"+o,f,fi) ++
//      filter("a"+o,"out"+o,ok) ++
//      sdrain("a"+o,"out"+o)
//    }
//    def genBSeqs(from:Int,to:Int): GCConnector = to match {
//      case 0 => empty()
//      case `from` => genB(to,to+1,id,success,id)
//      case _ if to>0 => genB(to,to+1,id,success,id) ++ genBSeqs(from,to-1)
//    }
//    def genBPars(i:Int): GCConnector = i match {
//      case 0 => empty()
//      case 1 => genB(1,2,id,success,id)
//      case _ if i>0 => genB(i,i+1,id,success,id) ++ genBPars(i-1)
//    }
//    
//    val conn = if (fst)
//      writer("out1",List(1)) ++
//      genB(1,2,id,fail,id) ++
//      genBSeqs(2,n)
//    else if (seq)        
//	  writer("out1",List(1)) ++
//      genBSeqs(1,n-1) ++
//      genB(n,n+1,id,fail,id)
//    else
//      writer("out1",List(1)) ++
//      genBPars(n)
//      
//    // Running the experiments //
//          
//    if (sat) {
//      val cons = conn.getConstraints
//      cons.close
//      val time = System.currentTimeMillis()
//      val res = cons.solveChocoSat
//      val spent = System.currentTimeMillis() - time
//      show(spent,"SAT",res)
//    }
//    else if (smt) {
//      val cons = conn.getConstraints
//      val time = System.currentTimeMillis()
//      val res = cons.solveChocoDyn
//      val spent = System.currentTimeMillis() - time
//      show(spent,"SMT",res)
//    } 
//    else {
//      printHelp
//      return
//    }
//  }
  
  /////////////////////////////
  /// Parallel asyncrhonous ///
  /////////////////////////////

  else if (args(0) startsWith "pairwise") {
    if (debug) println("Pairwise asynchronous")
	
    var const = Formula(True-->True)

	val isEven = Predicate("isEven") {
	  case i:Int => i%2 == 0
	}
	def genPair(i:Int): Node[GCSolution,Formula] = {
	  val conn =
       wiredWriter("x"+i,
          i) ++ 
          //for (n<-(1 to 50).toList) yield i) ++ 
       filter("x"+i,"y"+i,isEven) //++
//      reader("y"+i)
//      genReader(i)
      const ++= conn.getConstraints
      engine add (conn,priority = Set("x"+i) )
	}
    def connectPair(i:Int,n1:Node[GCSolution,Formula],n2:Node[GCSolution,Formula]) = {
      val conn = adrain("x"+i,"x"+(i+1))
      const ++= conn.getConstraints
      val ad = engine add conn
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
    
//    val time = System.currentTimeMillis()
//    engine.init
//    engine.awaitTermination
//    val spent = System.currentTimeMillis() - time
//    show(spent,if (all) "ALL" else if (part) "Partial"+workers else "OneByOne"+workers,NoneSol())
    
    
    if (sat) {
//      val cons = conn.getConstraints.close
      val cons = const.close
      val time = System.currentTimeMillis()
      val res = cons.solveChocoSat
      val spent = System.currentTimeMillis() - time
      engine.kill
      show(spent,"SAT",res)
    }
    else if (smt) {
//      val cons = conn.getConstraints.close
      val cons = const.close
      val time = System.currentTimeMillis()
      val res = cons.solveChocoDyn
      val spent = System.currentTimeMillis() - time
      engine.kill
      show(spent,"SMT",res)
    } 
    else if (all||part||obo) {
      val time = System.currentTimeMillis()
	  engine.init
	  engine.awaitTermination
	  val spent = System.currentTimeMillis() - time
	  show(spent,
	      (if (all) "ALL" else if (part) "Partial"+workers else "OneByOne"+workers)+
	      (if (pvt) "-0" else ""),NoneSol())
    }
    else {
      engine.kill
      printHelp
      return
    }    
    
  }
}}