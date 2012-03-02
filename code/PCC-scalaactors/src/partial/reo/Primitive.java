package partial.reo;

import choco.Choco;
import choco.kernel.model.constraints.Constraint;
import choco.kernel.model.variables.integer.IntegerVariable;
import partial.solver.Constr;
import partial.solver.Neg;
import partial.solver.Var;

import java.util.*;

/**
 * Created by IntelliJ IDEA.
 * User: dave
 * Date: 10/01/12
 * Time: 14:08
 * To change this template use File | Settings | File Templates.
 */
public abstract class Primitive {
    public abstract List<End> getEnds();

    public abstract Constr[] getConstraints();

//    public abstract Map<String, IntegerVariable> getVars();
//    public abstract void setVars(Map<String, IntegerVariable> vars);

    // never used: End.getNoFlowConstraint() used instead.
    public Constr[] getNoFlowConstraints() {
        Constr[] res = new Constr[getEnds().size()];
        int i=0;
        for (End var: getEnds()) {
            res[i++] = new Neg(new Var(var.getName()));
        }
        return res;
    }
    
    public Set<End> requires(End e) {
        return new HashSet<End>();
    }

    public void updateState() {}
    
    public void showConnector(Collection<Primitive> hist) {
        hist.add(this);
        System.out.println("> " + toString());
        for (End e: getEnds()) {
            if (e.isConnected())
                if (! hist.contains(e.getOther().getPrimitive())) {
                       e.getOther().getPrimitive().showConnector(hist);
                }
        }
    }

    protected abstract void buildConstraints();

//    /**
//     * Cleans the internal copy of integerVariables and rebuilds the constraints
//     */
//    public void renewConstraints() {
////        System.out.println("### renewing constraints of "+this);
//        setVars(new HashMap<String, IntegerVariable>());
//        buildConstraints();
//    }
    
    @Override
    public String toString() {
        String res = getClass().getSimpleName()+"[";
        boolean fst = true;
        for (End e: getEnds())
            if (fst) {
                fst = false;
                res = res + e.getName();
            }
            else res = res + "," + e.getName();
        return res + "]";
    }
}
