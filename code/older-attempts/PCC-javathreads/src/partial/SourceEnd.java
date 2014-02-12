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
public class SourceEnd extends End {
    private SinkEnd other = null;

    public SourceEnd(String name, Primitive primitive) {
        super(name, primitive);
    }

    public void connect(SinkEnd snk) {
        other = snk;
        if (!other.isConnected()) other.connect(this);
//        System.out.println("-- connecting -- "+getName()+"-"+snk.getName());
    }

    public boolean isConnected() {
        return other != null;
    }

    public End getOther() {
        return other;
    }

    public Constraint getConstraint() {
        if (isConnected())
            return Solver.equiv(Solver.var(getPrimitive().getVars(), getName()),
                            Solver.var(other.getPrimitive().getVars(), other.getName()));
        else return Choco.TRUE;
    }
}
