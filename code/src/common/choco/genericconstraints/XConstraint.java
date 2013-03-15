package common.choco.genericconstraints;

import choco.cp.solver.CPSolver;
import choco.cp.solver.variables.integer.IntVarEvent;
import choco.kernel.model.variables.integer.IntegerVariable;
import choco.kernel.solver.ContradictionException;
import choco.kernel.solver.Solver;
import choco.kernel.solver.constraints.AbstractSConstraint;
import choco.kernel.solver.constraints.SConstraintType;
import choco.kernel.solver.constraints.integer.AbstractIntSConstraint;
import choco.kernel.solver.constraints.integer.AbstractTernIntSConstraint;
import choco.kernel.solver.variables.integer.IntDomainVar;
import common.Function;
import common.Predicate;
import scala.Int;
import scala.Tuple2;
import scala.collection.Map;

import java.util.ArrayList;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 13/07/12
 * Time: 11:39
 * To change this template use File | Settings | File Templates.
 */
//public class XConstraint extends AbstractTernIntSConstraint {
public class XConstraint extends AbstractIntSConstraint {

    final CPSolver solver;
    final Map<Integer,Object> datahash;
    final Map<Integer,Tuple2<Function,IntegerVariable>> funchash;
    final Buffer buffer;
    final Predicate pred;
    private List<IntDomainVar> waiting = new ArrayList<IntDomainVar>();
    private boolean restarting;
    private boolean DEBUG_ = false;

    private void DEBUG(String str) {
        if (DEBUG_) System.out.println(str);
    }

    public XConstraint(CPSolver solver,
//                       IntDomainVar xpred,
//                       IntDomainVar xdata,
//                       IntDomainVar xnew,
                       IntDomainVar[] allVars, // size >=3  --- maybe a map to keep track of indices.
                       Map<Integer, Object> datahash,
                       Map<Integer, Tuple2<Function, IntegerVariable>> funchash,
                       Buffer buffer,
                       Predicate pred) {
//        rest.add(xpred);
//        super(xpred,xdata,xnew);
        super(allVars.length,allVars); //(IntDomainVar[]) allVars.toArray());
        this.solver = solver;
        this.datahash = datahash;
        this.funchash= funchash;
        this.buffer = buffer;
        this.pred = pred;
        this.restarting = false;
        if (allVars.length < 3)
            throw new RuntimeException("Need at least 3 variables");
        this.waiting.add(allVars[0]);
        this.waiting.add(allVars[1]);
        this.waiting.add(allVars[2]);
        DEBUG("### CREATING X-CONSTRAINTS, part 2:");
        if (DEBUG_)
            for (int i = 0; i < allVars.length; i++)
                System.out.print(allVars[i].getName()+"  ");
        DEBUG("");
//                    allVars[0].getName()+" - "+allVars[1].getName()+" - "+allVars[2].getName());
    }

    @Override
    public int getFilteredEventMask(int idx) {
        return IntVarEvent.INSTINT_MASK;
    }

    /** * Default initial propagation: full constraint re-propagation. */
    public void awake() throws ContradictionException {
        DEBUG("### AWAKE");
        propagate();
    }

    /**
     * default propagation on instantiation: full constraint re-propagation
     * @param idx index of the variable to reduce
     */
    public void awakeOnInst(int idx) throws ContradictionException {
        IntDomainVar v = getVar(idx);
        DEBUG("### AWAKEONINST ("+v.getName()+")");
        if (restarting) {
            DEBUG("restarting propagation...");
            restarting = false;
            propagate();
        }
        else if (waiting.remove(v)) {
            DEBUG("going in...");
            propagate();
        }
        else DEBUG("NOT going in...");
    }

    /** * <i>Propagation:</i> * Propagating the constraint until local consistency is reached.
     * @throws ContradictionException * contradiction exception */

    public void propagate() throws ContradictionException {

        // TODO: BUG -- running function, but sometimes not checking predicates

        DEBUG("### PROPAGATING!!");
        // if x as flow and ^x has a value
        if (getVar(0).isInstantiated() && getVar(1).isInstantiated())  // xpred,xdata instantiated
        if (getVar(0).getVal() == 1) {                          // xpred is true (flow on departure)
            DEBUG("Got in: "+getVar(0).getName()+" is true!");
            int next = getVar(1).getVal();
            boolean hasFunc = funchash.contains(next);
            ArrayList<Function> funcs = new ArrayList<Function>();
            while (hasFunc) {
                DEBUG("has func: "+next);
                hasFunc = false;
                Tuple2<Function,IntegerVariable> tpl = funchash.apply(next);
                funcs.add(tpl._1());
                IntDomainVar fv = solver.getVar(tpl._2());
                DEBUG("instantiated? "+fv.getName()+" - "+fv.isInstantiated());
                if (fv.isInstantiated()) {
                    next = fv.getVal();
                    hasFunc = funchash.contains(fv.getVal());
                }
                else {
                    DEBUG("waiting for: "+fv.getName());
                    waiting.add(fv); // add dependency: value of fv is needed to find out what data is there.
                }
                // if fv is not instantiated a dependent function was not evaluated... this causes bad behaviour:
                //  the solver thinks it is ok to chose any value uninstantiated vars,
                //  since fv is not an official var of 'this'.
                // SOLUTIONS: include fv in the official list, or maybe somehow modify fv? - fst!
                // NEW PROBLEM:
                // if it is instantiated, it never waits for anything else... It might be important.
                // Need green card to propagate if predicate becomes false!!!!!
            }
            // now either next is the next and not a function, or fv is not instantiated.
            // in both cases, next could be a data value and we can say yes or no, or no deal.
            // TO be more accurate, we could check if the fv.isInst failed...
            DEBUG("updating vars? next: "+next);
            if (datahash.contains(next)) {
                if (buffer.check(pred, funcs, datahash.apply(next)) == 0) {
                    DEBUG("setting "+getVar(2).getName()+" to false (and restarting prop)");
                    restarting = true;
                    getVar(2).removeVal(1,this,false);
                }
                 else {
                    DEBUG("setting "+getVar(2).getName()+" to true (also restarting - should only ask in a new branch(?)");
                    restarting = true;
                    getVar(2).removeVal(0,this,false);
                }
            }
        }
    }

    /**
     * Checks if the constraint is satisfied when the variables are instantiated.
     */
    @Override
    public boolean isSatisfied(int[] tuple) {
        DEBUG("### IsSatisfied? ("+getVar(0).getName()+" - "+getVar(1).getName()+") - called - unsure...");
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
                boolean res = tuple[2] == buffer.check(pred, funcs, datahash.apply(next));
                DEBUG("### RETURNING "+res);
                return res;
            }

            else{
                throw new RuntimeException("checking if satisfied, but no data can be inferred yet!");
            }
        }
    }


    public AbstractSConstraint<IntDomainVar> opposite(Solver solver) {
//        throw new RuntimeException("Lazy constraints can occur only in positive positions!");
        DEBUG("### OPOSITE");
        return new NegXConstraint(this);
    }

    public String pretty() {
        return pred.toString()+ "("+getVar(2).getName()+")";
    }

    private class NegXConstraint extends AbstractSConstraint<IntDomainVar> {

        final XConstraint hc;

        public NegXConstraint(XConstraint hc) {
            super(hc.vars);
            this.hc = hc;
            DEBUG("### CREATING NEGATION");
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
