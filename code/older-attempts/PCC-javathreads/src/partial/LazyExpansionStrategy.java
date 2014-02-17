package partial;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 06/02/12
 * Time: 15:04
 * To change this template use File | Settings | File Templates.
 */
public class LazyExpansionStrategy extends DefaultExpansionStrategy {

    private List<End> priority = new ArrayList<End>();

    /**
     * Initially all ends in the traversal are considered priority ones.
     * @param traversal
     * @return initial set of ends for traversing.
     */ 
    @Override
    public List<End> initial(Traversal traversal) {
        List<End> res = super.initial(traversal);
        priority.addAll(res);
        return res;
    }

    /**
     * Extends the super.expand by updating the priority list.
     * @param traversal
     * @param toExpand Expansion point.
     * @return The same as super.expand.
     */
    @Override
    public List<End> expand(Traversal traversal, End toExpand) {

        if (toExpand == null) return new ArrayList<End>();

        priority.remove(toExpand);
        List<End> res = super.expand(traversal, toExpand);
        // update priority list
        if (toExpand.isConnected())
            for (End req: toExpand.getOther().getPrimitive().requires(toExpand.getOther()))
                if (traversal.fringe.contains(req))
                    priority.add(req);
        return res;
    }


    
    /**
     * Checks if the traversal is ready, by checking if there is any priority end in the fringe.
     * @param traversal
     * @return True if the traversal is ready to search for solutions. 
     */
    @Override
    public boolean readyToSolve(Traversal traversal) {
        if (traversal.checkIfTryToSolve()) {
//            System.out.print("<smart-steal>");
            return true;
        }

        for (End e: priority)
            if (traversal.fringe.contains(e))
                return false;
        return true;
    }

    /**
     * Searches for priority ends before normal ends.
     * @param traversal The graph being traversed, with a fringe
     * @return
     */
    @Override
    public End next(Traversal traversal) {
        for (End e: priority)
            if (traversal.fringe.contains(e))
                return e;
        return super.next(traversal);
    }

    @Override
    public ExpansionStrategy createStrategy() {
        return new LazyExpansionStrategy();
    }
    
    
//    private DefaultExpansionStrategy defstr = new DefaultExpansionStrategy();
//    
//    
//    public List<End> initial(Traversal traversal) {
//        // assumes initial set of ends always needs to be expanded.
//        return expand(traversal);
//    }
//
//
//    public List<End> expand(Traversal traversal) {
//        List<End> res = new ArrayList<End>();
//        List<End> round = new ArrayList<End>();
//        List<End> priority = new ArrayList<End>();
//        End expandEnd;
//        
//        priority.add(defstr.next(traversal));
//        
//        while (!priority.isEmpty()) {
//            expandEnd = priority.remove(0); // pop
//            round = defstr.expand(traversal,expandEnd); // we now it will use expandEnd -- always in the fringe!
//            res.addAll(round);
//
//            if (expandEnd.isConnected())
//                for (End req: expandEnd.getOther().getPrimitive().requires(expandEnd.getOther()))
//                    if (traversal.fringe.contains(req))
//                        priority.add(req);
//
//        }
//
//        return res;
//    }
//        
//    
//    public List<End> expand_old(Traversal traversal) {
//        // only stops expanding if a solution is possible.
//        List<End> res = new ArrayList<End>();
//        Set<End> now = new HashSet<End>();
//
//        // get the next expansion point
//        End expansionPoint = null;
//        for (End e: traversal.fringe) {expansionPoint = e; break;}
//        // if there no ends in the fringe, return empty set;
//        if (expansionPoint==null) return res;
//
//        now.add(expansionPoint);
//
//        // keep expanding the "now" set
//        while (!now.isEmpty()) {
//            End nnext = null;
//            for (End e: now) {nnext = e; break;}
//            now.remove(nnext);
//            System.out.println("expanding "+nnext.getName());
//
//            List<End> tmpres = traversal.extend(nnext); // traversal extended here!
//            Set<End> tmpnow = null;
//            if(nnext.isConnected()) {
//                tmpnow = nnext.getOther().getPrimitive().requires(nnext.getOther());
////                System.out.print("adding requirements: ");
////                for (End nnow: tmpnow)
////                    System.out.print(nnow.getName()+", ");
////                System.out.println();
//            }
//
////            tmpnow.removeAll(res); // ...
////            tmpnow.removeAll(traversal.ends);
////            now.addAll(tmpnow);
////            for (End e: tmpnow) 
////                if (!traversal.ends.contains(e))
////                    now.add(e);
//
//            System.out.print("adding requirements: ");
//
//            // update fringe by adding new ends to its head, and dropping connected ports from the fringe
//            for (End primEnd: tmpres)
//                if (!traversal.fringe.contains(primEnd) && primEnd.isConnected()) {
//                    if (!traversal.ends.contains(primEnd.getOther())) {
//                        traversal.fringe.add(0,primEnd);
//                        if (tmpnow.contains(primEnd)) {
//                            now.add(primEnd);
//                            System.out.print(primEnd.getName()+", ");
//                        }
//                    }
//                    else traversal.fringe.remove(primEnd.getOther());
//                }
//            System.out.println();
//
//            res.addAll(tmpres);
//        }
//
//        return res;
//    }
}
