package partial;

import choco.Choco;
import choco.kernel.model.constraints.Constraint;

/**
 * Created by IntelliJ IDEA.
 * User: dave
 * Date: 10/01/12
 * Time: 14:13
 * To change this template use File | Settings | File Templates.
 */
public class SinkEnd extends End {
    private SourceEnd other = null;

    public SinkEnd(String name, Primitive primitive) {
        super(name, primitive);
    }

    public void connect(SourceEnd src) {
        other = src;
        if (!other.isConnected()) other.connect(this);
//        System.out.println("-- connecting -- "+getName()+"-"+src.getName());
    }

    public boolean isConnected() {
        return other != null;
    }
    
    public End getOther() {
        return other;
    }

    public Constraint getConstraint() {
        return Choco.TRUE;
    }
}
