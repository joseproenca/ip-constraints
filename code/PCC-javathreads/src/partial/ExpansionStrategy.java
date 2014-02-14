package partial;

import java.util.List;
import java.util.Set;

/**
 * Created by IntelliJ IDEA.
 * User: dave
 * Date: 15/01/12
 * Time: 11:02
 * To change this template use File | Settings | File Templates.
 */
public interface ExpansionStrategy {
    /** determine the initial ends to work with, base on some seed. */
    public List<End> initial(Traversal traversal);

    /** find some more primitives not included in set  */
    public List<End> expand(Traversal traversal);

    /** updates the fringe after adding new ends */
    public void updateFringe(Traversal traversal, List<End> newEnds);

    /** checks if it is worth searching now for a solution. */
    public boolean readyToSolve(Traversal traversal);

    public ExpansionStrategy createStrategy();
}
