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
public class LossySync extends Primitive {
    private SourceEnd src;
    private SinkEnd snk;
    private List<End> ends = new ArrayList<End>();

    private Map<String, IntegerVariable> vars = new HashMap<String, IntegerVariable>();
    private Constraint[] constraints;

    public LossySync() {
        this(GenSym.gensym(), GenSym.gensym());
    }

    public LossySync(String src, String snk) {
        this.src = new SourceEnd(src, this);
        this.snk = new SinkEnd(snk, this);
        ends.add(this.src);
        ends.add(this.snk);

        buildConstraints();
    }

    protected void buildConstraints() {
        // b->a  &  [!a -> (!b & !a_ & b^)  &  !b -> ((a & !b^) \/ !a)]
        constraints = new Constraint[]{
                impl(var(vars, snk.getName()), var(vars, src.getName()))};
//            impl(notvar(vars, src), and(notvar(vars, snk), notvar(vars, mkSrc(src)), var(vars, mkSnk(snk)))),
//            impl(notvar(vars, mkSrc(src)), or(and(var(vars, src), notvar(vars, mkSnk(snk))), notvar(vars, src)))};
    }

    public SourceEnd getSource() {
        return src;
    }

    public SinkEnd getSink() {
        return snk;
    }

    @Override
    public Set<End> requires(End e) {
        // naive implementation
        Set<End> res = new HashSet<End>();
        if (e == snk) res.add(src); // context independent: only snk depends on src.
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
