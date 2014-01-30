package reopp.common.benchmarks

import reopp.workers.strategies.GenDeployer
import reopp.workers.Deployer
import reopp.common.guardedcommands.dataconnectors.ConnectorGen._
import reopp.common.guardedcommands.GCConnector
import reopp.common.Function
import reopp.common.Predicate
import scala.util.Random

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
  
  val n = Integer.parseInt(args(1))
  val sat = args(2) startsWith "sat"
  val smt = args(2) startsWith "smt"
  val ahc = args(2) startsWith "ahc"
  val all = args(2) startsWith "smt"
  val p1 = args(2) startsWith "partial1"
  val p2 = args(2) startsWith "partial2"
  val p4 = args(2) startsWith "partial4"
  val print = args(2) startsWith "print"

//  val deployer =
//	 if      (p1) GenDeployer.hybrid(1)
//	 else if (p2) GenDeployer.hybrid(2)
//	 else if (p4) GenDeployer.hybrid(4)
//	 else         GenDeployer.all(1)
//                 // up to 2 workers
//  deployer.start()
  
  
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
      
    if (print) {
    	val cons = conn.getConstraints
	    println(cons)
        println(cons.solveChocoX)
    }
    
    
    
  }


}