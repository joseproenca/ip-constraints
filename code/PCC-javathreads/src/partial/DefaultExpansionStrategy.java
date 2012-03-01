package partial;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Created by IntelliJ IDEA.
 * User: dave
 * Date: 15/01/12
 * Time: 11:13
 * To change this template use File | Settings | File Templates.
 */
public class DefaultExpansionStrategy implements ExpansionStrategy {
    public List<End> initial(Traversal traversal) {
        return traversal.ends;
    }

    /** Find some more primitives not included in set - returns just the new ends.
     * Gets only the ends of the primitive of ONE end, and the fringe acts as a stack.
     * @post traversal gets completely expanded with new ends "res". */
    public List<End> expand(Traversal traversal, End toExpand) {
        // default traversal: depth first
        List<End> res = new ArrayList<End>();

        if (toExpand == null) return res;
        if (!traversal.fringe.contains(toExpand)) {
            System.out.println("### something went wrong... expansion point not in the fringe! " + toExpand.getName());
            return res;
        }
        
        //for (End next: toExpand) { //traversal.fringe) {
//        List<End> next = new ArrayList<End>();
//        next.add(toExpand);
        res = traversal.extend(toExpand);
        // update fringe by adding new ends to its head, and dropping connected ports from the fringe
        updateFringe(traversal,res);

        //            return res;
        // if there are no ends in the fringe, return empty set;
        return res;
    }

    public void updateFringe(Traversal traversal, List<End> newEnds) {
        for (End newEnd: newEnds)
            if (!traversal.fringe.contains(newEnd) && newEnd.isConnected()) {
                if (!traversal.ends.contains(newEnd.getOther()))
                    traversal.fringe.add(0,newEnd);
                else traversal.fringe.remove(newEnd.getOther());
            }
    }

    public List<End> expand(Traversal traversal) {
        return expand(traversal,next(traversal));
    }

    public boolean readyToSolve(Traversal traversal) {
        return true;
    }

    public ExpansionStrategy createStrategy() {
        return new DefaultExpansionStrategy();
    }


    /**
     * Get the next ends to be expanded
     * @param traversal The graph being traversed, with a fringe
     * @return Ends to be expanded
     */
    public End next(Traversal traversal) {
        //List<End> res = new ArrayList<End>();
        End res = null;
        for (End next: traversal.fringe) {
            res = next; //traversal.extend(next);
            break;
        }
        return res;
    }
}
