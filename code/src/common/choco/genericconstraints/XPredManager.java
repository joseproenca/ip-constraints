package common.choco.genericconstraints;

import choco.cp.model.managers.IntConstraintManager;
import choco.cp.solver.CPSolver;
import choco.kernel.model.constraints.ComponentConstraint;
import choco.kernel.model.constraints.Constraint;
import choco.kernel.model.variables.integer.IntegerVariable;
import choco.kernel.solver.Solver;
import choco.kernel.solver.constraints.SConstraint;
import common.Function;
import common.Predicate;
import scala.Tuple2;
import scala.collection.Map;

import java.util.ArrayList;
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
            return new XConstraint((CPSolver) solver,
                    solver.getVar(variables[0]),solver.getVar(variables[1]),solver.getVar(variables[2]),
                    ((ArrayList<Map<Integer,Object>>) parameters).get(0),
                    ((ArrayList<Map<Integer,Tuple2<Function,IntegerVariable>>>) parameters).get(1),
                    ((ArrayList<Buffer>) parameters).get(2),
                    ((ArrayList<Predicate>) parameters).get(3)
            );
        }
        return null;
    }


    /// STATIC AUXILIARY ///


    static public Constraint genConstr(IntegerVariable xpred, IntegerVariable xdata, IntegerVariable xnew, Map<Integer,Object> datahash, Map<Integer,Tuple2<Function,IntegerVariable>> funchash, Buffer buf, Predicate pred) {
//        System.out.println("Creating generic predicate - "+xpred.getName()+" - "+pred);
        ArrayList<Object> parameters = new ArrayList<Object>();
        parameters.add(0,datahash);
        parameters.add(1,funchash);
        parameters.add(2,buf);
        parameters.add(3,pred);
        return new ComponentConstraint(XPredManager.class, parameters, new IntegerVariable[]{xpred,xdata,xnew});
    }
}