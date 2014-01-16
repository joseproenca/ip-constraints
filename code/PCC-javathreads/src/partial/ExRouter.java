package partial;

import choco.Choco;
import choco.kernel.model.constraints.Constraint;
import choco.kernel.model.variables.integer.IntegerVariable;

import java.util.*;

import static partial.sat.Solver.*;

/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 11/01/12
 * Time: 16:10
 * To change this template use File | Settings | File Templates.
 */
public class ExRouter extends Primitive {
    List<End> srcs = new ArrayList<End>();
    List<End> snks = new ArrayList<End>();

    private Map<String, IntegerVariable> vars = new HashMap<String, IntegerVariable>();
    private Constraint[] constraints;

    public ExRouter(Collection<String> srcs, Collection<String> snks) {
        for (String src : srcs)
            this.srcs.add(new SourceEnd(src, this));
        for (String snk : snks)
            this.snks.add(new SourceEnd(snk, this));

        updateConstraints(srcs, snks);
    }

    public void addSource(String end) {
        srcs.add(new SourceEnd(end, this));
        buildConstraints();
    }

    public void addSink(String end) {
        srcs.add(new SinkEnd(end, this));
        buildConstraints();
    }

    protected void buildConstraints() {
        List<String> str_srcs = new ArrayList<String>();
        List<String> str_snks = new ArrayList<String>();
        for (End e : srcs)
            str_srcs.add(e.getName());
        for (End e : snks)
            str_snks.add(e.getName());
        updateConstraints(str_srcs, str_snks);
    }

    private void updateConstraints(Collection<String> srcs, Collection<String> snks) {
        // sum(src) <= 1  &  sum(snk) <= 1  &  sum(src) = sum(snk)
        constraints = new Constraint[]{
                Choco.leq(varsum(vars, srcs), 1),
                Choco.leq(varsum(vars, snks), 1),
                Choco.eq(varsum(vars, srcs), varsum(vars, snks))};
    }

    public End[] getSources() {
        return srcs.toArray(new End[0]);
    }

    public End[] getSinks() {
        return snks.toArray(new End[0]);
    }

    @Override
    public Set<End> requires(End e) {
        // naive implementation
        Set<End> res = new HashSet<End>();
        if (srcs.contains(e))
            for (End snk: snks) {
                res.add(snk); break;
            }
        if (snks.contains(e))
            for (End src: srcs) {
                res.add(src); break;
            }
        return res;
    }
    
    @Override
    public List<End> getEnds() {
        List<End> res = new ArrayList<End>();
        res.addAll(srcs);
        res.addAll(snks);
        return res;
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
