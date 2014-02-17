package partial;

import choco.Choco;
import choco.kernel.model.constraints.Constraint;
import partial.sat.Solver;

/**
 * Created by IntelliJ IDEA.
 * User: dave
 * Date: 10/01/12
 * Time: 14:12
 * To change this template use File | Settings | File Templates.
 */
public abstract class End {
    private String name;
    private Primitive primitive;

    public End(String name, Primitive primitive) {
        this.name = name;
        this.primitive = primitive;
    }

    public String getName() {
        return name;
    }

    public Primitive getPrimitive() {
        return primitive;
    }

    abstract boolean isConnected();
    abstract End getOther();

    abstract Constraint getConstraint();
    
    public Constraint getNoFlowConstraint() {
        // var may still not exist yet!
        return Choco.eq(Solver.getvar(primitive.getVars(),name),0);
    }

    public boolean isReady() {
        return false;
    }
}
