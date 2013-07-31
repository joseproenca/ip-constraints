package reopp.common.choco.genericconstraints;

import choco.cp.model.managers.IntConstraintManager;
import choco.cp.solver.CPSolver;
import choco.kernel.model.constraints.ComponentConstraint;
import choco.kernel.model.constraints.Constraint;
import choco.kernel.model.constraints.ConstraintType;
import choco.kernel.model.constraints.MetaConstraint;
import choco.kernel.model.variables.VariableType;
import choco.kernel.model.variables.integer.IntegerExpressionVariable;
import choco.kernel.model.variables.integer.IntegerVariable;
import choco.kernel.solver.Solver;
import choco.kernel.solver.constraints.SConstraint;
import reopp.common.Buffer;
import reopp.common.Function;
import reopp.common.Predicate;

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


    //// AUXILIARY TO PRINT CONSTRAINTS!!
    // TODO: move to another class
    // Adds parenthesis for readability if there are *spaces* in the string.
    private static String mbParenthesis(String s) {
        if (s.contains(" "))
            return "(" + s + ")";
        else
            return s;
    }

    public static String prettyConst(Constraint c){
//    System.out.println( "["+c.getClass()+"] "+c.pretty());
        if (c instanceof MetaConstraint) {
            MetaConstraint<?> mc = (MetaConstraint<?>) c;
            if (mc.getConstraintType() == ConstraintType.IMPLIES)
                return mbParenthesis(prettyConst(mc.getConstraint(0)))+" -> "+mbParenthesis(prettyConst(mc.getConstraint(1)));
            if (mc.getConstraintType() == ConstraintType.AND)
                return mbParenthesis(prettyConst(mc.getConstraint(0)))+" /\\ "+mbParenthesis(prettyConst(mc.getConstraint(1)));
            if (mc.getConstraintType() == ConstraintType.OR)
                return mbParenthesis(prettyConst(mc.getConstraint(0)))+" \\/ "+mbParenthesis(prettyConst(mc.getConstraint(1)));
            if (c.getConstraintType() == ConstraintType.NOT)
                return "not "+mbParenthesis(prettyConst(mc.getConstraint(0)));
            //output.println("I'm a imply!\nleft: "+mc.getConstraint(0).pretty());
        }
        if (c instanceof ComponentConstraint) {
            ComponentConstraint cc = (ComponentConstraint) c;
            //return cc.getVariable(0) + "[[]]";
            if (c.getConstraintType() == ConstraintType.EQ)
                if (c.pretty().endsWith("[0, 1], 1 } )"))
                     return "`"+cc.getVariable(0).getName()+"`";
                else return prettyVar(cc.getVariable(0))+" = "+prettyVar(cc.getVariable(1));
            if (c.getConstraintType() == ConstraintType.GEQ)
                return prettyVar(cc.getVariable(0))+" >= "+prettyVar(cc.getVariable(1));
            if (c.getConstraintType() == ConstraintType.LEQ)
                return prettyVar(cc.getVariable(0))+" <= "+prettyVar(cc.getVariable(1));
            if (c.getConstraintType() == ConstraintType.GT)
                return prettyVar(cc.getVariable(0))+" > "+prettyVar(cc.getVariable(1));
            if (c.getConstraintType() == ConstraintType.LT)
                return prettyVar(cc.getVariable(0))+" < "+prettyVar(cc.getVariable(1));
        }
        return //"["+c.getClass()+"] "+c.pretty();
                c.pretty();
    }

    private static String prettyVar(choco.kernel.model.variables.Variable v) {
//      System.out.println( "--["+v+": "+v.getClass()+"] "+v.pretty());
        if (v instanceof IntegerExpressionVariable) {
            IntegerExpressionVariable exp = (IntegerExpressionVariable) v;

            if (exp.getOperator().name() == "SUM") {
                String res = "";
                if (exp.getNbVars() > 0) {
                    res += prettyVar(exp.getVariable(0));
                    for (int i=1; i<exp.getNbVars(); i++)
                        res += " + "+prettyVar(exp.getVariable(i));
                }
                else
                    res = "0";
                return res;
            }
        }
        if (v instanceof IntegerVariable) {
            IntegerVariable iv = (IntegerVariable) v;
            if (iv.isBoolean()) return "'"+iv.getName()+"'";
            if (iv.getVariableType() == VariableType.INTEGER) return "'"+iv.getName()+"'";
        }
        return v.pretty();
    }

}