package common.beh.choco.genericconstraints;

import choco.cp.solver.variables.integer.IntVarEvent;
import choco.kernel.solver.ContradictionException;
import choco.kernel.solver.Solver;
import choco.kernel.solver.constraints.AbstractSConstraint;
import choco.kernel.solver.constraints.integer.AbstractUnIntSConstraint;
import choco.kernel.solver.variables.integer.IntDomainVar;

import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 13/07/12
 * Time: 17:56
 * To change this template use File | Settings | File Templates.
 */

// TODO: FIX NEGATION! (not (xf -> xpred)   <->   xf /\ not xpred)  - NOT WORKING YET
class NegLazyPredSConstraint extends AbstractUnIntSConstraint {

  final Object data;
  final Buffer buffer;
  final UnPredicate predicate;
  final List<UnFunction> functions;
  final IntDomainVar xflow;
  final IntDomainVar yflow;

    public NegLazyPredSConstraint(IntDomainVar xpred,
                                IntDomainVar xflow, // for information, not for restriction
                                IntDomainVar yflow, // for information, not for restriction
                                Object data,
                                Buffer buffer,
                                UnPredicate predicate,
                                List<UnFunction> functions) {
    super(xpred);
    this.data = data;
    this.buffer = buffer;
    this.predicate = predicate;
    this.xflow = xflow;
    this.yflow = yflow;
    this.functions = functions;
  }

  @Override
  public int getFilteredEventMask(int idx) {
    return IntVarEvent.INSTINT_MASK;
  }

  /** * Default initial propagation: full constraint re-propagation. */
  public void awake() throws ContradictionException {
//      if (true) throw new RuntimeException("Lazy constraints can occur only in positive positions!");
//      System.out.println("AWAKE (Neg-Pred) - dom size xpred/xflow "+v0.getDomainSize()+"/"+xflow.getDomainSize());

//      DisposableIntIterator it = v0.getDomain().getIterator();
//      try{
//              // xpred must be 0 - remove other option
//              while(it.hasNext()){
//                  int val = it.next();
//                  System.out.println("AWAKE (Neg-Pred) - filtering xpred "+val);
//                  if (val == 1)
//                      v0.removeVal(val,this,false);
////                  System.out.println("dom: "+v0.getDomainSize());
//              }
//
////              // xflow must be 1 - remove other option
////              it = xflow.getDomain().getIterator();
////              while (it.hasNext()) {
////                  int val = it.next();
////                  System.out.println("AWAKE (Neg-Pred) - filtering xflow "+val);
////                  if (val == 0)
////                      xflow.removeVal(val,this,false);
////                  System.out.println("dom: "+v0.getDomainSize());
////              }

//          System.out.println("AWAKE (Neg-Pred) - dom size "+v0.getDomainSize());
//      }finally {
//          it.dispose();
//      }

//      // xpred must be false (== 0)
//      System.out.println("AWAKE (Neg-Pred) - filtering xpred from being 1");
//      v0.removeVal(1, this, false);
////      System.out.println("AWAKE (Neg-Pred) - filtered  xpred from being 1 - dom "+v0.getDomainSize() +"/"+ v0.getVal());
////      if (v0.getDomainSize() == 0) fail();
//
//      // predicate must be false (== 0)
//      int check = buffer.check(predicate,functions,data); // negation of predicate
//      System.out.println("AWAKE (Neg-Pred) - checking predicate that must be false (0) - "+check);
//      if (check == 1) fail();

      // xpred must be different from predicate
      int check = buffer.check(predicate,functions,data);
      if (check == 0)
          v0.removeVal(0,this,false);
      else
          v0.removeVal(1,this,false);
  }

  /** * <i>Propagation:</i>
   *  Propagating the constraint until local consistency is reached.
   *  @throws ContradictionException
   *  contradiction exception
   */
  public void propagate() throws ContradictionException {
//      if(true) throw new RuntimeException("Lazy constraints can occur only in positive positions!");
//      System.out.println("PROPAGATING (Neg-Pred) - dom size xpred/xflow "+v0.getDomainSize()+"/"+xflow.getDomainSize());

//      // xflow cannot be instantiated with 0
//      if(xflow.isInstantiated()) {
//          System.out.println("PROPAGATING (Neg-Pred) - xflow is instantiated with "+xflow.getVal());
//          if (xflow.getVal() == 0)
//              fail();
//      }

//      // xpred cannot be instantiated with 1, and predicate cannot be true (== 1)
//      if(v0.isInstantiated()){
//          System.out.println("PROPAGATING (Neg-Pred) - xpred is instantiated with "+v0.getVal());
//          //            if(!predicate.check(v0.getVal())) {
////          if (buffer.check(predicate,data) == v0.getVal() ) { // checking negation!
////              fail();
////          }
//          if (v0.getVal() == 1)
//              fail();
//          else if (buffer.check(predicate,functions,data) == 1)
//              fail();
//      }
      if(v0.isInstantiated()){
          if (buffer.check(predicate,functions,data) == v0.getVal())
              fail();
      }
  }

  /**
   * Checks if the constraint is satisfied when the variables are instantiated.
   */
  @Override
  public boolean isSatisfied(int[] tuple) {
//    if (true) throw new RuntimeException("Lazy constraints can occur only in positive positions!");
    return (tuple[0] != buffer.check(predicate,functions,data));
  }


  public AbstractSConstraint<IntDomainVar> opposite(Solver solver) {
      return new LazyPredSConstraint(v0,xflow,yflow,data,buffer,predicate,functions);
  }

  public String pretty() {
    return "!"+predicate.toString()+ "("+data+")";
  }

}
