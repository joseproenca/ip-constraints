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
public class PredManager extends IntConstraintManager {
    public SConstraint makeConstraint(Solver solver,
                                      IntegerVariable[] variables,
                                      Object parameters,
                                      List<String> options) {
        if (solver instanceof CPSolver) {
//            System.out.println("creating new lazy predicate with data "+((ArrayList<Object>)parameters).get(0));
            return new LazyPredSConstraint(solver.getVar(variables[0]),solver.getVar(variables[1]),solver.getVar(variables[2]),
//                    solver.getVar(((ArrayList<IntegerVariable>) parameters).get(3)),
                    ((ArrayList<Object>) parameters).get(0),
                    ((ArrayList<Buffer>) parameters).get(1),
                    ((ArrayList<Predicate>) parameters).get(2),
                    ((ArrayList<List<Function>>) parameters).get(3)
            );
        }
        return null;
    }


    /// STATIC AUXILIARY ///


    static public Constraint genConstr(IntegerVariable xpred, IntegerVariable xflow, IntegerVariable yflow, Object d, Buffer buf, Predicate pred, List<Function> funcs) {
//        System.out.println("Creating generic predicate - "+xpred.getName()+" - "+pred);
        ArrayList<Object> parameters = new ArrayList<Object>();
        parameters.add(0,d);
        parameters.add(1,buf);
        parameters.add(2,pred);
        parameters.add(3,funcs);
        return new ComponentConstraint(PredManager.class, parameters, new IntegerVariable[]{xpred,xflow,yflow});
    }
}