package partial;

import choco.kernel.model.constraints.Constraint;

import static partial.sat.Solver.notvar;

/**
 * Created by IntelliJ IDEA.
 * User: dave
 * Date: 14/01/12
 * Time: 16:20
 * To change this template use File | Settings | File Templates.
 */
public class ContinuousWriter extends Writer {
    private Constraint[] constraints;
    protected int limit = -1;

    public ContinuousWriter(int counter) {
        this();
        limit = counter;
        if (limit==0) buildConstraints();
    }

    public ContinuousWriter() {
        this(GenSym.gensym());
    }

    public ContinuousWriter(String snk) {
        this.snk = new SinkEnd(snk,this) {
            @Override
            public boolean isReady() {
                return limit != 0;
            }
        };
        ends.add(this.snk);
        buildConstraints();
    }



    public void updateState() {
        int oldlimit = limit;
        if (limit > 0) limit--;
        if (oldlimit == 1) buildConstraints();
//        System.out.println("Data sent <"+toString()+"Counter: "+oldlimit+"--"+limit+">");
    }

    @Override
    protected void buildConstraints() {
        if (limit == 0)
            constraints = new Constraint[]{notvar(getVars(), snk.getName())};
        else
            constraints = new Constraint[]{};
    }

    public Constraint[] getConstraints() {
        return constraints;
    }

}
