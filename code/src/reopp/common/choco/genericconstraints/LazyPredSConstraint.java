package reopp.common.choco.genericconstraints;

import choco.cp.solver.variables.integer.IntVarEvent;
import choco.kernel.solver.ContradictionException;
import choco.kernel.solver.Solver;
import choco.kernel.solver.constraints.AbstractSConstraint;
import choco.kernel.solver.constraints.integer.AbstractTernIntSConstraint;
import choco.kernel.solver.variables.integer.IntDomainVar;
import reopp.common.Buffer;
import reopp.common.Function;
import reopp.common.Predicate;

import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 13/07/12
 * Time: 11:39
 * To change this template use File | Settings | File Templates.
 */
public class LazyPredSConstraint extends AbstractTernIntSConstraint {

    final Object data;
    final Buffer buffer;
    final Predicate predicate;
    final List<Function> functions;
//    final IntDomainVar xflow;

    public LazyPredSConstraint(IntDomainVar xpred,
                               IntDomainVar xflow, // for information, not for restriction
                               IntDomainVar yflow, // for information, not for restriction
                               Object data,
                               Buffer buffer,
                               Predicate predicate,
                               List<Function> functions) {
        super(xpred,xflow,yflow);
        this.data = data;
        this.buffer = buffer;
        this.predicate = predicate;
        this.functions = functions;
//        this.xflow = xflow;
    }

    @Override
    public int getFilteredEventMask(int idx) {
        return IntVarEvent.INSTINT_MASK;
    }

    /** * Default initial propagation: full constraint re-propagation. */
    public void awake() throws ContradictionException {
//        System.out.println("# AWAKE ("+v0.getName()+"/"+v1.getName()+") - dom size xpred/xflow "+v0.getDomainSize()+"/"+v1.getDomainSize());

        // 3 options for xflow:
        //  - instantiated with 1 - xpred and pred must be true
        //  - instantiated with 0 - xpred can be anything (don't care about pred)
        //  - uninstantiated - wait for instantiation?

        // if xflow == 1, then - predicate and xpred must coincide
        // ----- OLD: (1) predicate must be true and (2) xpred must be true
        if (v1.isInstantiated() && v2.isInstantiated())
            if (v1.getVal() == 1 && v2.getVal() == 1) {
//                DisposableIntIterator it = v0.getDomain().getIterator();
//                try{
//                    // (1) predicate must be true
//                    int check = buffer.check(predicate,functions,data);
//                    System.out.println("fail? - "+(check == 0));
//                    if (check==0) fail();
//                    else
//                    // (2) xpred must be true (filter xpred == 0)
//                    while(it.hasNext()){
//                        int val = it.next();
//                        System.out.println("AWAKE (Pred) - filtering "+val);
////                        if (check == 0 || val == 0)
////                            v0.removeVal(val,this,false);
//                        if (val == 0)
//                            v0.removeVal(val,this,false);
//                        System.out.println("dom: "+v0.getDomainSize());
//            //                if(val % 2 != 0){
//            //                    v0.removeVal(val, this, false);
//            //                }
//                    }
//                    System.out.println("AWAKE (Pred) - dom size "+v0.getDomainSize());
//                }finally {
//                    it.dispose();
//                }
                int check = buffer.check(predicate,functions,data);
                if (check == 0)
                    v0.removeVal(1,this,false);
                else
                    v0.removeVal(0,this,false);
            }


//        DisposableIntIterator it = v0.getDomain().getIterator();
//        try{
//            while(it.hasNext()){
//                int val = it.next();
//                System.out.println("AWAKE (UnPred) - solving - filtering "+val);
////                if(val % 2 != 0){
////                    v0.removeVal(val, this, false);
////                }
//            }
//            System.out.println("AWAKE (UnPred) - dom size "+v0.getDomainSize());
//        }finally {
//            it.dispose();
//        }
    }

    /**
     * default propagation on instantiation: full constraint re-propagation
     * @param idx index of the variable to reduce
     */
    public void awakeOnInst(int idx) throws ContradictionException {
        if (idx == 1) {
//            System.out.println("# AWAKE (Pred) - xflow instantiated! - "+v1.getVal());
            propagate();
        }
        if (idx == 2) { propagate(); }

        //constAwake(false); // change if necessary
//        propagate();
    }

    /** * <i>Propagation:</i> * Propagating the constraint until local consistency is reached. * * @throws ContradictionException * contradiction exception */

    public void propagate() throws ContradictionException {
//        System.out.println("# PROPAGATING (Pred) - dom size xpred/xflow "+v0.getDomainSize()+"/"+v1.getDomainSize());

        // if xflow == 1, then predicate and xpred must coincide -----predicate must be true and "1 in domain_xpred"
        if(v1.isInstantiated() && v2.isInstantiated())
        if(v1.getVal() == 1 && v2.getVal() == 1) {
            if (buffer.check(predicate,functions,data) == 0)
                v0.removeVal(1,this,false);
            else
                v0.removeVal(0,this,false);
//                fail();
//            else if (v0.isInstantiated()){
//                System.out.println("PROPAGATING (UnPred) - is instantiated with "+v0.getVal());
////                if(!predicate.check(v0.getVal())) {
//                if (v0.getVal() == 0) {
//                    fail();
//                }
//            }
        }
    }

    /**
     * Checks if the constraint is satisfied when the variables are instantiated.
     */
    @Override
    public boolean isSatisfied(int[] tuple) {
//        System.out.println("# IsSatisfied? ("+v0.getName()+" - "+v1.getName()+") - called");
        if (tuple[1] == 0 || tuple[2] == 0) return true;
        else return (tuple[0] ==  buffer.check(predicate,functions,data));
    }


    public AbstractSConstraint<IntDomainVar> opposite(Solver solver) {
//        throw new RuntimeException("Lazy constraints can occur only in positive positions!");
        return new NegLazyPredSConstraint(v0,v1,v2,data,buffer,predicate,functions);
    }

    public String pretty() {
        return predicate.toString()+ "("+data+")";
    }

}
