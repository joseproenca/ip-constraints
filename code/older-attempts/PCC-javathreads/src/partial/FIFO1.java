package partial;

import choco.kernel.model.constraints.Constraint;
import choco.kernel.model.variables.integer.IntegerVariable;

import java.util.*;

import static partial.sat.Solver.notvar;
import static partial.sat.Solver.var;

/**
 * Created by IntelliJ IDEA.
 * User: dave
 * Date: 10/01/12
 * Time: 14:22
 * To change this template use File | Settings | File Templates.
 */
public class FIFO1 extends Primitive {
    private SourceEnd src;
    private SinkEnd snk;
    private boolean full;
    private List<End> ends = new ArrayList<End>();

    private Map<String, IntegerVariable> vars = new HashMap<String, IntegerVariable>();
    private Constraint[] constraints_full, constraints_empty;


    public FIFO1(boolean full) {
        this(GenSym.gensym(), GenSym.gensym(), full);
    }

    public FIFO1(String src, String snk, boolean full) {
        this.full = full;
        this.src = new SourceEnd(src, this){
            @Override
            public boolean isReady() {
                return !((FIFO1) getPrimitive()).isFull();
            }
        };
        this.snk = new SinkEnd(snk, this){
            @Override
            public boolean isReady() {
                return ((FIFO1) getPrimitive()).isFull();
            }
        };
        ends.add(this.src);
        ends.add(this.snk);

        buildConstraints();
    }

    protected void buildConstraints() {
        // !a
        constraints_full = new Constraint[]{notvar(vars, this.src.getName())};
        // !b
        constraints_empty = new Constraint[]{var(vars, this.src.getName())};
    }

    public SourceEnd getSource() {
        return src;
    }

    public SinkEnd getSink() {
        return snk;
    }

    public void updateState(boolean full) {
        this.full = full;
    }

    public boolean isFull() {
        return full;
    }

    @Override
    public void updateState() {
        full = !full;
    }

    @Override
    public List<End> getEnds() {
        return ends;
    }

    @Override
    public Constraint[] getConstraints() {
        if (full) return constraints_full;
        else return constraints_empty;

    }

    @Override
    public Map<String, IntegerVariable> getVars() {
        return vars;
    }
    @Override
    public void setVars(Map<String, IntegerVariable> vars) {
        this.vars = vars;
    }
}
