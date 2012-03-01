package partial;

import choco.Choco;
import choco.kernel.model.constraints.Constraint;
import choco.kernel.model.variables.integer.IntegerVariable;
import partial.sat.Solver;

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

    public abstract Constraint[] getConstraints();

    public abstract Map<String, IntegerVariable> getVars();
    public abstract void setVars(Map<String, IntegerVariable> vars);

    // never used: End.getNoFlowConstraint() used instead.
    public Constraint[] getNoFlowConstraints() {
        Constraint[] res = new Constraint[getVars().size()];
        int i=0;
        for (IntegerVariable var: getVars().values()) {
            res[i++] = Choco.eq(var,0);
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

    /**
     * Cleans the internal copy of integerVariables and rebuilds the constraints
     */
    public void renewConstraints() {
//        System.out.println("### renewing constraints of "+this);
        setVars(new HashMap<String, IntegerVariable>());
        buildConstraints();
    }
    
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
