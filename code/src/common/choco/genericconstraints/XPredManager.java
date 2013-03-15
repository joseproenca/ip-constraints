package common.choco.genericconstraints;

import choco.cp.model.managers.IntConstraintManager;
import choco.cp.solver.CPSolver;
import choco.kernel.model.constraints.ComponentConstraint;
import choco.kernel.model.constraints.Constraint;
import choco.kernel.model.variables.integer.IntegerVariable;
import choco.kernel.solver.Solver;
import choco.kernel.solver.constraints.SConstraint;
import choco.kernel.solver.variables.integer.IntDomainVar;
import common.Function;
import common.Predicate;
import scala.Int;
import scala.Tuple2;
import scala.collection.Map;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 13/07/12
 * Time: 11:53
 * To change this template use File | Settings | File Templates.
 */
@SuppressWarnings("unchecked")
public class XPredManager extends IntConstraintManager {
    public SConstraint makeConstraint(Solver solver,
                                      IntegerVariable[] variables,
                                      Object parameters,
                                      List<String> options) {
        if (solver instanceof CPSolver) {
//            System.out.println("creating new lazy predicate with data "+((ArrayList<Object>)parameters).get(0));
            IntDomainVar[] allVars = new IntDomainVar[variables.length]; // variables.map(solver.getVar(_))
            for (int i = 0; i < variables.length; i++) {
                allVars[i] = solver.getVar(variables[i]);
            }
            return new XConstraint((CPSolver) solver,
                    allVars,
//                    solver.getVar(variables[0]),solver.getVar(variables[1]),solver.getVar(variables[2]),
                    ((ArrayList<Map<Integer,Object>>) parameters).get(0),
                    ((ArrayList<Map<Integer,Tuple2<Function,IntegerVariable>>>) parameters).get(1),
                    ((ArrayList<Buffer>) parameters).get(2),
                    ((ArrayList<Predicate>) parameters).get(3)
            );
        }
        return null;
    }


    /// STATIC AUXILIARY ///


    static private Constraint genConstrOld(IntegerVariable xpred, IntegerVariable xdata, IntegerVariable xnew, Map<Integer,Object> datahash, Map<Integer,Tuple2<Function,IntegerVariable>> funchash, Buffer buf, Predicate pred) {
//        System.out.println("Creating generic predicate - "+xpred.getName()+" - "+pred);
        ArrayList<Object> parameters = new ArrayList<Object>();
        parameters.add(0,datahash);
        parameters.add(1,funchash);
        parameters.add(2,buf);
        parameters.add(3,pred);
        System.out.println("### CREATING X-CONSTRAINTS");
        return new ComponentConstraint(XPredManager.class, parameters, new IntegerVariable[]{xpred,xdata,xnew});
    }

    /**
     * Generates a customised constraint, assuming funchash is COMPLETE!
     * @param xpred
     * @param xdata
     * @param xnew
     * @param datahash
     * @param funchash
     * @param buf
     * @param pred
     * @return
     */
    static public Constraint genConstr(
            IntegerVariable xpred, IntegerVariable xdata, IntegerVariable xnew,
//            List<IntegerVariable> rest,
            Map<Integer,Object> datahash, Map<Integer,Tuple2<Function,IntegerVariable>> funchash,
            Buffer buf, Predicate pred) {
        ArrayList<Object> parameters = new ArrayList<Object>();
        parameters.add(0,datahash);
        parameters.add(1,funchash);
        parameters.add(2,buf);
        parameters.add(3,pred);
        // collecting all integer variables: first xpred, xdata, xnew, then all function dependencies.
        List<IntegerVariable> rest = new ArrayList<IntegerVariable>();
        rest.add(xpred);
        rest.add(xdata);
        rest.add(xnew);
        scala.collection.Iterator<Tuple2<Function, IntegerVariable>> it = funchash.valuesIterator();
        while (it.hasNext()) {
            rest.add(it.next()._2());
        }
//        it = funchash.valuesIterator();
//        System.out.println("------ "+funchash.iterator().size());
//        while (it.hasNext()) {
//            System.out.println("------ "+it.next()._2());
//        }

//        System.out.println("### CREATING X-CONSTRAINTS");
        return new ComponentConstraint(XPredManager.class, parameters,
                rest.toArray(new IntegerVariable[rest.size()]));
    }
}