package partial;

import choco.kernel.model.constraints.Constraint;
import choco.kernel.model.variables.integer.IntegerVariable;

import java.util.*;

/**
 * Created by IntelliJ IDEA.
 * User: dave
 * Date: 14/01/12
 * Time: 16:09
 * To change this template use File | Settings | File Templates.
 */
public abstract class Writer extends Primitive {
    protected SinkEnd snk;
    protected List<End> ends = new ArrayList<End>();

    private Map<String, IntegerVariable> vars = new HashMap<String, IntegerVariable>();

//    public Writer() {
//        this(GenSym.gensym());
//    }
//
//    public Writer(String snk) {
//        this.snk = new SinkEnd(snk, this);
//        ends.add(this.snk);
//    }

    public SinkEnd getSink() {
        return snk;
    }

    @Override
    public List<End> getEnds() {
        return ends;
    }

    @Override
    abstract public Constraint[] getConstraints();

    @Override
    public Map<String, IntegerVariable> getVars() {
        return vars;
    }
    @Override
    public void setVars(Map<String, IntegerVariable> vars) {
        this.vars = vars;
    }
}
