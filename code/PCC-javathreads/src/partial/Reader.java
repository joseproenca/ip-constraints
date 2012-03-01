package partial;

import choco.kernel.model.constraints.Constraint;
import choco.kernel.model.variables.integer.IntegerVariable;

import java.util.*;

/**
 * Created by IntelliJ IDEA.
 * User: dave
 * Date: 14/01/12
 * Time: 16:19
 * To change this template use File | Settings | File Templates.
 */
public abstract class Reader extends Primitive {
    protected SourceEnd src;
    protected List<End> ends = new ArrayList<End>();

    private Map<String, IntegerVariable> vars = new HashMap<String, IntegerVariable>();

//    public Reader() {
//        this(GenSym.gensym());
//    }
//
//    public Reader(String src) {
//        this.src = new SourceEnd(src, this);
//        ends.add(this.src);
//    }

    public SourceEnd getSource() {
        return src;
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
