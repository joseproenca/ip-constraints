package partial;

import java.util.*;

/**
 * Created by IntelliJ IDEA.
 * User: dave
 * Date: 22/01/12
 * Time: 16:21
 * To change this template use File | Settings | File Templates.
 */
public class CompleteExpansionStrategy  implements ExpansionStrategy {
    public List<End> initial(Traversal traversal) {
        // get all ends of the connector  (based on the fringe)
        List<End> res = new ArrayList<End>();
        Queue<End> queue = new LinkedList<End>();
//        System.out.println("collecting ALL ends...");
        for (End e: traversal.fringe)
            for (End e2: e.getPrimitive().getEnds())
                if (!queue.contains(e2)) queue.add(e2);
        
        while (!queue.isEmpty()) {
            End next = queue.remove();
            res.add(next);
//            System.out.print(next.toString()+" ,");
            if (next.isConnected())
                if (!res.contains(next.getOther()) && !queue.contains(next.getOther()))
                    queue.addAll(next.getOther().getPrimitive().getEnds());
        }
        traversal.ends = res;
        traversal.fringe = new ArrayList<End> ();
        
//        System.out.println("\nCollected all ends of the connector.");
            
        return res;

    }

    /** Find some more primitives not included in set - returns just the new ends.
     * Side-effect: traversal gets completely expanded with new ends "res". */
    public List<End> expand(Traversal traversal) {
        // there should be no other ends to collect.
        return new ArrayList<End>();
    }

    public void updateFringe(Traversal traversal, List<End> newEnds) {
    }

    public boolean readyToSolve(Traversal traversal) {
        return true;
    }

    public ExpansionStrategy createStrategy() {
        return new CompleteExpansionStrategy();
    }
}
