package partial;

import choco.kernel.model.constraints.Constraint;
import choco.kernel.model.variables.integer.IntegerVariable;

import java.util.*;

import static partial.sat.Solver.*;

/**
 * Created by IntelliJ IDEA.
 * User: dave
 * Date: 11/01/12
 * Time: 15:41
 * To change this template use File | Settings | File Templates.
 */
public class SyncDrain extends Primitive {
    private SourceEnd src1, src2;
    private List<End> ends = new ArrayList<End>();

    //    private partial.sat.CNF constr;
    private Map<String, IntegerVariable> vars = new HashMap<String, IntegerVariable>();
    private Constraint[] constraints;

    public SyncDrain() {
        this(GenSym.gensym(), GenSym.gensym());
    }


    public SyncDrain(String src1, String src2) {
        this.src1 = new SourceEnd(src1, this);
        this.src2 = new SourceEnd(src2, this);
        ends.add(this.src1);
        ends.add(this.src2);
        buildConstraints();


    }

    protected void buildConstraints() {
        //    Integer[][] constr_cnf = {{-1,3}, {-3,1}, {1,-4,-2}, {1,-4,4}, {1,2,-2}, {1,2,4}};
//    String[] const_vars = {src, Solver.mkSrc(src),snk,Solver.mkSnk(snk)};
//    constr = new CNF(constr_cnf,const_vars);

        // a <-> b   [&  a \/ !a_ \/ !b^]
        constraints = new Constraint[]{
                vareq(vars, src1.getName(), src2.getName())};
//        or(var(vars, src1), not(var(vars, mkSrc(src1))), not(var(vars, mkSnk(src2))))};
    }


    public SourceEnd getSource1() {
        return src1;
    }

    public SourceEnd getSource2() {
        return src2;
    }

    @Override
    public Set<End> requires(End e) {
        // naive implementation
        Set<End> res = new HashSet<End>();
        if      (e == src2) res.add(src1);
        else if (e == src1) res.add(src2);
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
