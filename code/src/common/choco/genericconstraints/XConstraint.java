package common.choco.genericconstraints;

import choco.cp.solver.CPSolver;
import choco.cp.solver.variables.integer.IntVarEvent;
import choco.kernel.model.variables.integer.IntegerVariable;
import choco.kernel.solver.ContradictionException;
import choco.kernel.solver.Solver;
import choco.kernel.solver.constraints.AbstractSConstraint;
import choco.kernel.solver.constraints.SConstraintType;
import choco.kernel.solver.constraints.integer.AbstractTernIntSConstraint;
import choco.kernel.solver.variables.integer.IntDomainVar;
import common.Function;
import common.Predicate;
import scala.Tuple2;
import scala.collection.Map;

import java.util.ArrayList;

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 13/07/12
 * Time: 11:39
 * To change this template use File | Settings | File Templates.
 */
public class XConstraint extends AbstractTernIntSConstraint {

    final CPSolver solver;
    final Map<Integer,Object> datahash;
    final Map<Integer,Tuple2<Function,IntegerVariable>> funchash;
    final Buffer buffer;
    final Predicate pred;

    public XConstraint(CPSolver solver,
                       IntDomainVar xpred,
                       IntDomainVar xdata,
                       IntDomainVar xnew,
                       Map<Integer, Object> datahash,
                       Map<Integer, Tuple2<Function, IntegerVariable>> funchash,
                       Buffer buffer,
                       Predicate pred) {
        super(xpred,xdata,xnew);
        this.solver = solver;
        this.datahash = datahash;
        this.funchash= funchash;
        this.buffer = buffer;
        this.pred = pred;
    }

    @Override
    public int getFilteredEventMask(int idx) {
        return IntVarEvent.INSTINT_MASK;
    }

    /** * Default initial propagation: full constraint re-propagation. */
    public void awake() throws ContradictionException {
        propagate();
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

    /** * <i>Propagation:</i> * Propagating the constraint until local consistency is reached.
     * @throws ContradictionException * contradiction exception */

    public void propagate() throws ContradictionException {

        // if x as flow and ^x has a value
        if (v0.isInstantiated() && v1.isInstantiated())
        if (v0.getVal() == 1) {
            Integer next = v1.getVal();
            boolean hasFunc = funchash.contains(next);
            ArrayList<Function> funcs = new ArrayList<Function>();
            while (hasFunc) {
                hasFunc = false;
                Tuple2<Function,IntegerVariable> tpl = funchash.apply(next);
                funcs.add(tpl._1());
                IntDomainVar fv = solver.getVar(tpl._2());
                if (fv.isInstantiated()) {
                    next = fv.getVal();
                    hasFunc = funchash.contains(fv.getVal());
                }
            }
            // now either next is the next and not a function, or fv is not instantiated.
            // in both cases, next could be a data value and we can say yes or no, or no deal.
            // TO be more accurate, we could check if the fv.isInst failed...
            if (datahash.contains(next)) {
                if (buffer.check(pred, funcs, datahash.apply(next)) == 0)
                    v2.removeVal(1,this,false);
                 else
                    v2.removeVal(0,this,false);
            }
        }
    }

    /**
     * Checks if the constraint is satisfied when the variables are instantiated.
     */
    @Override
    public boolean isSatisfied(int[] tuple) {
//        System.out.println("# IsSatisfied? ("+v0.getName()+" - "+v1.getName()+") - called");
        if (tuple[0] == 0) return true;
        else {
            Integer next = tuple[1];
            boolean hasFunc = funchash.contains(next);
            ArrayList<Function> funcs = new ArrayList<Function>();
            while (hasFunc) {
                hasFunc = false;
                Tuple2<Function,IntegerVariable> tpl = funchash.apply(next);
                funcs.add(tpl._1());
                IntDomainVar fv = solver.getVar(tpl._2());
                if (fv.isInstantiated()) {
                    next = fv.getVal();
                    hasFunc = funchash.contains(fv.getVal());
                }
            }
            // now either next is the next and not a function, or fv is not instantiated.
            // in both cases, next could be a data value and we can say yes or no, or no deal.
            // TO be more accurate, we could check if the fv.isInst failed...
            if (datahash.contains(next)) {
                return tuple[2] == buffer.check(pred, funcs, datahash.apply(next));
            }

            else{
                throw new RuntimeException("checking if satisfied, but no data can be inferred yet!");
            }
        }
    }


    public AbstractSConstraint<IntDomainVar> opposite(Solver solver) {
//        throw new RuntimeException("Lazy constraints can occur only in positive positions!");
        return new NegXConstraint(this);
    }

    public String pretty() {
        return pred.toString()+ "("+v2.getName()+")";
    }

    private class NegXConstraint extends AbstractSConstraint<IntDomainVar> {

        final XConstraint hc;

        public NegXConstraint(XConstraint hc) {
            super(hc.vars);
            this.hc = hc;
        }

        public void propagate() throws ContradictionException {
            throw new RuntimeException("negation of a HashSConstraint should NOT be propagated!");
        }

        @Override
        public boolean isConsistent() {
            throw new RuntimeException("negation of a HashSConstraint should NOT be checked for isConsistent!");
        }

        public boolean isSatisfied() {
            throw new RuntimeException("negation of a HashSConstraint should NOT be checked for isSatisfied!");
        }

        public AbstractSConstraint<IntDomainVar> opposite(Solver solver) {
//        throw new RuntimeException("Lazy constraints can occur only in positive positions!");
            return hc;
        }

        public SConstraintType getConstraintType() {
            throw new RuntimeException("negation of a HashSConstraint should NOT be asked for its Constraint Type!");
        }

    }
}
