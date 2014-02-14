package partial.sat;

import choco.Choco;
import choco.cp.model.CPModel;
import choco.cp.solver.CPSolver;
import choco.kernel.common.logging.ChocoLogging;
import choco.kernel.common.logging.Verbosity;
import choco.kernel.model.constraints.Constraint;
import choco.kernel.model.variables.integer.IntegerExpressionVariable;
import choco.kernel.model.variables.integer.IntegerVariable;

import java.util.*;

/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 11/01/12
 * Time: 10:31
 * To change this template use File | Settings | File Templates.
 */
public class Solver {

    public CPSolver s;

//    /** CNF is now deprecated... (that was fast!)
//     *
//     * @param cnf
//     * @deprecated
//     */
//    public void solve(CNF cnf) {
//        CPModel m = new CPModel();
//
////        Boolean verbose = false;
////        if (!verbose) ChocoLogging.setVerbosity(Verbosity.OFF);
//
//        HashMap<String,IntegerVariable> chocovars = new HashMap<String, IntegerVariable>();
//        for (String var: cnf.varnames)
//            chocovars.put(var,Choco.makeBooleanVar(var));
//
//        for (Integer[] cnfclause: cnf.clauses) {
//            Constraint[] clause = new Constraint[cnfclause.length];
//            for (int i=0; i<clause.length; i++) {
//                String name = cnf.varnames.get(Math.abs(cnfclause[i])-1);
//                //clause[i] = Choco.makeBooleanVar(name);
//                if (cnfclause[i] > 0) clause[i] = Choco.eq(chocovars.get(name),1);
//                else                  clause[i] = Choco.eq(chocovars.get(name),0);
//            }
//            m.addConstraint(Choco.or(clause));
//        }
//
//        // Build the solver
//        CPSolver s = new CPSolver();
//        // Read the model
//        s.read(m);
//        // Solve the model
//        s.solve();
//
//        // print the problem
//        System.out.println(m.pretty());
//        // print the solution
//        System.out.println(s.pretty());
//    }

    public boolean solve(Collection<Constraint> cs) {
        ChocoLogging.setVerbosity(Verbosity.OFF);

        CPModel m = new CPModel();
        for (Constraint c : cs)
            m.addConstraint(c);
            //m.addConstraint(Choco.FALSE);

        // Build the solver
//        CPSolver
        s = new CPSolver();
        // Read the model
        s.read(m);
        // Solve the model

        Boolean result = s.solve();

        // print the problem
        //System.out.println(m.pretty());
        // print the solution
        //System.out.println(s.pretty());

        return (result == null ? false : result.booleanValue());
    }

    public boolean check(IntegerVariable v) {
        return s.getVar(v).getVal() != 0;
    }
    
    public String pretty() {
//        for (IntegerVariable c:
        String res = "";
        Iterator<IntegerVariable> v = s.getModel().getIntVarIterator();
        while (v.hasNext()) {
            IntegerVariable var = v.next();
            res += var.getName() + " -> " + s.getVar(var).getVal() + "\n";
        }
        return res;
    }
    
    public String showConstraints() {
        return s.getModel().pretty();
    }

    public static String mkSrc(String var) {
        return var + "$src";
    }

    public static String mkSnk(String var) {
        return var + "$snk";
    }

    public static IntegerVariable getvar(Map<String, IntegerVariable> m, String s) {
        IntegerVariable v;
        if (m.containsKey(s)) v = m.get(s);
        else {
            v = Choco.makeBooleanVar(s);
            m.put(s, v);
        }
        return v;
    }

    public static Constraint var(Map<String, IntegerVariable> m, String s) {
        return Choco.eq(getvar(m, s), 1);
    }

    public static Constraint notvar(Map<String, IntegerVariable> m, String s) {
        return Choco.eq(getvar(m, s), 0);
    }

    public static Constraint and(Constraint... n) {
        return Choco.and(n);
    }

    public static Constraint or(Constraint... n) {
        return Choco.or(n);
    }

    public static Constraint not(Constraint n) {
        return Choco.not(n);
    }

    public static Constraint impl(Constraint c1, Constraint c2) {
        return Choco.implies(c1, c2);
    }

    public static Constraint equiv(Constraint c1, Constraint c2) {
        return and(impl(c1, c2), impl(c2, c1));
    }

    public static Constraint vareq(Map<String, IntegerVariable> m, String s1, String s2) {
        return Choco.eq(getvar(m, s1), getvar(m, s2));
    }

    public static IntegerExpressionVariable varsum(Map<String, IntegerVariable> m, Collection<String> ss) {
        IntegerVariable[] vars = new IntegerVariable[ss.size()];
        Iterator<String> siter = ss.iterator();
        for (int i = 0; i < ss.size(); i++)
            vars[i] = getvar(m, siter.next());
        return Choco.sum(vars);
    }


    // Currently only experiments with choco below...
    public static void main(String[] args) {

        CPModel m = new CPModel();
        Boolean verbose = true;

        if (!verbose) ChocoLogging.setVerbosity(Verbosity.OFF);

        IntegerVariable i1 = Choco.makeIntVar("i1", -10, 100); //m.addVariable(i1);

        IntegerVariable b1 = Choco.makeBooleanVar("b1"); //m.addVariable(b1);
        IntegerVariable b2 = Choco.makeBooleanVar("b2"); //m.addVariable(b1);


        m.addConstraint(
                Choco.or(Choco.eq(b1, 1), Choco.eq(b2, 1)) // b1 && b2
        );

        // Build the solver
        CPSolver s = new CPSolver();

        if (verbose)
            System.out.println("####" + s.getConfiguration().stringPropertyNames());

        // print the problem
        if (verbose)
            System.out.println(m.pretty());
        // Read the model
        s.read(m);
        // Solve the model
        s.solve();
//        try {
//            s.getVar(b1).setVal(1);
//            //      s.getVar(b2).setVal(0);
//            //      s.getVar(b3).setVal(1);
//            System.out.println("$$$ check sol: " + s.checkSolution() + " $$$");
//            System.out.println("$$$ b1: " + s.getVar(b1).isInstantiated() + " $$$");
//            System.out.println("$$$ b2: " + s.getVar(b2).isInstantiated() + " $$$");
//        }
//        catch (Exception e){
//            System.out.println("$$$ Failed to check solution... $$$");
//            //       e1.printStackTrace();
//        }
        // print the solution
        if (verbose)
            System.out.println(s.pretty());
    }
}