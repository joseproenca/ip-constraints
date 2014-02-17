package partial;

import choco.Choco;
import choco.kernel.model.constraints.Constraint;

import static partial.sat.Solver.notvar;
import static partial.sat.Solver.vareq;

/**
 * Created by IntelliJ IDEA.
 * User: dave
 * Date: 14/01/12
 * Time: 16:34
 * To change this template use File | Settings | File Templates.
 */
public class ContinuousReader extends Reader {
    private Constraint[] constraints;
    protected int limit = -1;

    public ContinuousReader(int counter) {
        this();
        limit = counter;
        if (limit==0) buildConstraints();
    }

    public ContinuousReader() {
        this(GenSym.gensym());
    }

    public ContinuousReader(String snk) {
        this.src = new SourceEnd(snk,this) {
            @Override
            public boolean isReady() {
                return limit != 0;
            }
        };
        ends.add(this.src);
        buildConstraints();
    }

//    public ContinuousReader(String src) {
//        super(src);
//        constraints = new Constraint[]{};
//    }

    public void updateState() {
        int oldlimit = limit;
//        System.out.print("x");
        if (limit > 0) limit--;
        if (oldlimit == 1) buildConstraints();
//        System.out.println("Data received <" + toString() + ",counter:" + oldlimit + "--"+limit+">");
    }

    @Override
    protected void buildConstraints() {
        if (limit == 0)
            constraints = new Constraint[]{notvar(getVars(), src.getName())};
        else
            constraints = new Constraint[]{};
    }

    public Constraint[] getConstraints() {
        return constraints;
    }
}
