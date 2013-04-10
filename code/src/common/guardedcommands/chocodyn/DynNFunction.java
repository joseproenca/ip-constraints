package common.guardedcommands.chocodyn;

import choco.cp.solver.variables.integer.IntVarEvent;
import choco.kernel.solver.ContradictionException;
import choco.kernel.solver.Solver;
import choco.kernel.solver.constraints.AbstractSConstraint;
import choco.kernel.solver.constraints.SConstraintType;
import choco.kernel.solver.constraints.integer.AbstractIntSConstraint;
import choco.kernel.solver.variables.integer.IntDomainVar;
import common.Buffer;
import common.Function;

import java.util.ArrayList;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * <p/>
 * Created by jose on 10/04/13.
 */
public class DynNFunction extends AbstractIntSConstraint {

    final DataMap dm;
    final Buffer buffer;
    final Function function;

    // x = f(y1,y2,..)  --->  x,^x,y1,^y1,y2,^y2,...
    public DynNFunction( IntDomainVar[] vars,
                        DataMap data,
                        Buffer buffer,
                        Function function) {
        super(vars.length,vars);  // "length" is priority... not sure what are the consequences.
        this.dm = data;
        this.buffer = buffer;
        this.function = function;
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
        propagate();
//        if (idx == 0 || idx == 1 ) {
////            System.out.println("# AWAKE (Pred) - xflow instantiated! - "+v1.getVal());
//            propagate();
//        }
//        if (idx == 1) { propagate(); }
    }

    /** * <i>Propagation:</i> * Propagating the constraint until local consistency is reached. * * @throws ContradictionException * contradiction exception */

    public void propagate() throws ContradictionException {
//        System.out.println("# PROPAGATING (x = f(y)) - "+getVar(1).getName()+" = f("+getVar(3).getName()+") - x? ^x? y? ^y? "+
//                getVar(0).isInstantiated()+" "+getVar(1).isInstantiated()+" "+getVar(2).isInstantiated()+" "+getVar(3).isInstantiated());

        // ^x := f (^yn) -- if x,yn,^yn instantiated, x,yn have flow, instantiate ^x

        // x instantiated
        if(!getVar(0).isInstantiated()) return;
        // each yn and ^yn is instantiated
        for (int i=2; i<getNbVars(); i++)
            if (! getVar(i).isInstantiated()) return;
        // x and each yn has flow (== 1)
        for (int i=0; i<getNbVars(); i+=2)
            if (getVar(i).getVal() == 0) return;

        // calculate function value
        // order of objects seems to be right!
        List<Object> l = new ArrayList<Object>();
        for (int i=3; i<getNbVars(); i+=2)
            l.add(dm.get(getVar(i).getVal()));

        Object newVal = buffer.calculate(function,l);

//                System.out.println("defining "+getVar(1).getName()+" from f -> "+newVal+" -> idx "+getVar(3).getVal()+"");
        if (getVar(1).isInstantiated()) {
//                    System.out.println("already defined as "+dm.get(getVar(1).getVal())+"...");
            if (dm.get(getVar(1).getVal()) != newVal) fail();
        }
        else
            getVar(1).setVal(dm.add(newVal));
    }

    /**
     * Checks if the constraint is satisfied when the variables are instantiated.
     */
    @Override
    public boolean isSatisfied(int[] tuple) {
//        System.out.println("# IsSatisfied Func? ("+getVar(0).getName()+" - "+getVar(1).getName()+") - called");
        // x = F or y = F or ^x = f (^y)
        for (int i=0; i<getNbVars(); i+=2)
            if (tuple[i] == 0) return true;

        List<Object> l = new ArrayList<Object>();
        for (int i=3; i<getNbVars(); i+=2)
            l.add(dm.get(tuple[i]));

        Object newVal = buffer.calculate(function,l);
        return (dm.get(tuple[1]) == newVal);
    }


    public AbstractSConstraint<IntDomainVar> opposite(Solver solver) {
//        throw new RuntimeException("Lazy constraints can occur only in positive positions!");
//        DEBUG("### OPOSITE");
        return new NegDynNFunction(this);
    }

//    public AbstractSConstraint<IntDomainVar> opposite(Solver solver) {
////        throw new RuntimeException("Lazy constraints can occur only in positive positions!");
//        return new NegLazyPredSConstraint(v0,v1,v2,data,buffer,predicate,functions);
//    }

    public String pretty() {
        return function.toString()+ "("+getVar(0).getName()+")";
    }


    private class NegDynNFunction extends AbstractSConstraint<IntDomainVar> {

        final DynNFunction hc;

        public NegDynNFunction(DynNFunction hc) {
            super(hc.vars);
            this.hc = hc;
//            DEBUG("### CREATING NEGATION");
        }

        public void propagate() throws ContradictionException {
            throw new RuntimeException("negation of a DynNFunction should NOT be propagated!");
        }

        @Override
        public boolean isConsistent() {
            throw new RuntimeException("negation of a DynNFunction should NOT be checked for isConsistent!");
        }

        public boolean isSatisfied() {
            throw new RuntimeException("negation of a DynNFunction should NOT be checked for isSatisfied!");
        }

        public AbstractSConstraint<IntDomainVar> opposite(Solver solver) {
//        throw new RuntimeException("Lazy constraints can occur only in positive positions!");
            return hc;
        }

        public SConstraintType getConstraintType() {
            throw new RuntimeException("negation of a DynPredicate should NOT be asked for its Constraint Type!");
        }

    }

}