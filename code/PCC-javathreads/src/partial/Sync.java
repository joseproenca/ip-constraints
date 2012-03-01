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
public class Sync extends Primitive {
    private SourceEnd src;
    private SinkEnd snk;
    private List<End> ends = new ArrayList<End>();

    //    private partial.sat.CNF constr;
    private Map<String, IntegerVariable> vars = new HashMap<String, IntegerVariable>();
    private Constraint[] constraints;

    public Sync() {
        this(GenSym.gensym(), GenSym.gensym());
    }

    public Sync(String src, String snk) {
        this.src = new SourceEnd(src, this);
        this.snk = new SinkEnd(snk, this);
        ends.add(this.src);
        ends.add(this.snk);

        buildConstraints();
    }

    protected void buildConstraints() {
        //    Integer[][] constr_cnf = {{-1,3}, {-3,1}, {1,-4,-2}, {1,-4,4}, {1,2,-2}, {1,2,4}};
//    String[] const_vars = {src, Solver.mkSrc(src),snk,Solver.mkSnk(snk)};
//    constr = new CNF(constr_cnf,const_vars);

        // a <-> b  [&  a \/ !a_ \/ !b^]
        constraints = new Constraint[]{
                vareq(vars, this.src.getName(), this.snk.getName())};
//        or(var(vars, src), not(var(vars, mkSrc(src))), not(var(vars, mkSnk(snk))))};
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
        if      (e == snk) res.add(src);
        else if (e == src) res.add(snk);
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
