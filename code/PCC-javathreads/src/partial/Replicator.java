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
public class Replicator extends Primitive {

    private SourceEnd src;
    private SinkEnd snk1, snk2;
    private List<End> ends = new ArrayList<End>();

    private Map<String, IntegerVariable> vars = new HashMap<String, IntegerVariable>();
    private Constraint[] constraints;


    public Replicator() {
        this(GenSym.gensym(), GenSym.gensym(), GenSym.gensym());
//        System.out.println("new replicator with ends: "+src.getName()+","+snk1.getName()+","+snk2.getName()+",");
    }

    public Replicator(String src, String snk1, String snk2) {
        this.src = new SourceEnd(src, this);
        this.snk1 = new SinkEnd(snk1, this);
        this.snk2 = new SinkEnd(snk2, this);
        ends.add(this.src);
        ends.add(this.snk2);
        ends.add(this.snk1);
        buildConstraints();


    }

    protected void buildConstraints() {
        // a <-> b  & b <-> c
        constraints = new Constraint[]{
                vareq(vars, this.src.getName(), this.snk1.getName()),
                vareq(vars, this.src.getName(), this.snk2.getName()),};
    }

    public SourceEnd getSource() {
        return src;
    }

    public SinkEnd getSink1() {
        return snk1;
    }

    public SinkEnd getSink2() {
        return snk2;
    }

    @Override
    public Set<End> requires(End e) {
        // naive implementation
        Set<End> res = new HashSet<End>();
        if (e == src) {
            res.add(snk1);
            res.add(snk2);
        }
        else if (e == snk1) {
            res.add(snk2); res.add(src);
        }
        else if (e == snk2) {
            res.add(snk1); res.add(src);
        }
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
