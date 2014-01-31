package reopp.common.benchmarks

import reopp.workers.strategies.GenDeployer
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
object SCP14 extends App {
  
  //  Warmup.go

  if (args.size < 3) {
    println("""
Wrong usage. It should be: SCP14 <connector> <size> <method>
Possible connectors: "sensors", "transitions-spar", "transitions-sseq",... 
Possible methods: "sat", "smt", "ahc", "all", "partial1", "partial2", "partial4" 
""")
    exit
  }
  
  args(1)="500"
  args(2)="smt"
  args(3)=""
  
  val n = Integer.parseInt(args(1))
  val sat = args(2) startsWith "sat"
  val smt = args(2) startsWith "smt"
  val ahc = args(2) startsWith "ahc"
  val all = args(2) startsWith "smt"
  val p1 = args(2) startsWith "partial1"
  val p2 = args(2) startsWith "partial2"
  val p4 = args(2) startsWith "partial4"
  val print = (args.size > 3) && (args(3) startsWith "print")

//  val deployer =
//	 if      (p1) GenDeployer.hybrid(1)
//	 else if (p2) GenDeployer.hybrid(2)
//	 else if (p4) GenDeployer.hybrid(4)
//	 else         GenDeployer.all(1)
//                 // up to 2 workers
//  deployer.start()
  
  private def show(time:Long,mode:String,res:OptionSol[Solution]) {
      if (print) { 
        if (res.isDefined) println(mode+" - solved in "+time+" ms:\n"+res.get)
        else println(mode+" - no solution (in "+time+" ms)")
      }
      else println(mode+"        - "+time)            
  }
  
  //////////////////////////
  /// Sensors experiment ///
  //////////////////////////

  if (args(0) startsWith "sensors") {
    println("Sensors")
    
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
  
  // Transactions //
  else if (args(0) startsWith "transitions-spar") {
    println("transitions-spar")
  }

  


}