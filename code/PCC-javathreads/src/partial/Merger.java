package partial;

import choco.kernel.model.constraints.Constraint;
import choco.kernel.model.variables.integer.IntegerVariable;

import java.util.*;

import static partial.sat.Solver.*;

/**
 * Created by IntelliJ IDEA.
 * User: dave
 * Date: 10/01/12
 * Time: 14:23
 * To change this template use File | Settings | File Templates.
 */
public class Merger extends Primitive {
    private SourceEnd src1, src2;
    private SinkEnd snk;
    private List<End> ends = new ArrayList<End>();

    private Map<String, IntegerVariable> vars = new HashMap<String, IntegerVariable>();
    private Constraint[] constraints;

    public Merger() {
        this(GenSym.gensym(), GenSym.gensym(), GenSym.gensym());
    }

    public Merger(String src1, String src2, String snk) {
        this.src1 = new SourceEnd(src1, this);
        this.src2 = new SourceEnd(src2, this);
        this.snk = new SinkEnd(snk, this);
        ends.add(this.src2);
        ends.add(this.src1);
        ends.add(this.snk);

        buildConstraints();
    }

    protected void buildConstraints() {
        // (a \/ b) <-> c  &  !(a & b)
        constraints = new Constraint[]{
                equiv(var(vars, this.snk.getName()), or(var(vars, this.src1.getName()), var(vars, this.src2.getName()))),
                not(and(var(vars, this.src1.getName()), var(vars, this.src2.getName())))};
    }

    public SourceEnd getSource1() {
        return src1;
    }

    public SourceEnd getSource2() {
        return src2;
    }

    public SinkEnd getSink() {
        return snk;
    }

    @Override
    public Set<End> requires(End e) {
        // naive implementation
        Set<End> res = new HashSet<End>();
        if (e == snk) res.add(src1);
        if (e == src1 || e == src2) res.add(snk);
        return res;
    }

    @Override
    public List<End> getEnds() {
        return ends;
    }

    @Override
    public Constraint[] getConstraints() {
        return constraints;
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
