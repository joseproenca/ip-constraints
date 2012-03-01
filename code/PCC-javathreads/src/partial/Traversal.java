package partial;

import java.util.*;

/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 17/01/12
 * Time: 08:22
 * To change this template use File | Settings | File Templates.
 */
public class Traversal {
    public List<End> ends;
    public List<End> fringe; // order is used to decide how to expand
    private boolean tryToSolve = false;

    Traversal() {
        this(new ArrayList<End>(), new ArrayList<End>());
    }


    Traversal(List<End> ends, List<End> fringe) {
        this.ends = ends;
        this.fringe = fringe;
    }

    /** Add the ends of the primitive attached to a given end to the traversal,
     * and remove the given expanded ends from the fringe.
     * Do NOT add ends the fringe (strategy decides this).  */
    public List<End> extend(End borderend) {
        List<End> res = new ArrayList<End>();
        if (borderend.isConnected()) {
            res.addAll(borderend.getOther().getPrimitive().getEnds());
            fringe.remove(borderend.getOther()); // remove other end from fringe if it exists
        }
        fringe.remove(borderend);
        ends.addAll(res);
        return res;
    }

    /** Add the primitives of a set of ends to the traversal, but NOT add ends to the fringe.  */
    public List<End> extend(Collection<End> newends) {
        List<End> res = new ArrayList<End>();
        for (End e: newends) res.addAll(extend(e));
        return res;
//        ends.addAll(newends);
//        fringe.removeAll(newends);
//        for (End e1: newends) for (End e2: e1.getPrimitive().getEnds())
//            if (!ends.contains(e2)) fringe.add(e2);
    }

    public void dropEnds(Collection<End> ends) {
        this.ends.removeAll(ends);
    }

    public void setTryToSolve() {
        System.out.print("<TSolve>");
        tryToSolve = true;
    }

    public boolean checkIfTryToSolve() {
        if (tryToSolve) {
            tryToSolve = false;
            return true;
        }
        return false;
    }
    
    @Override
    public String toString() {
        String res = "Ends:   ";
        for (End e: ends) res = res + e.getPrimitive().getClass().getSimpleName() + "." + e.getName()+", ";
        res = res + "\nFringe: ";
        for (End e: fringe) res = res + e.getPrimitive().getClass().getSimpleName() + "." + e.getName()+", ";
        return res;
    }


//    /** MyStragegy: Extend only one end from the fringe, and delay the extension of other ends (depth first) */
//    public Set<End> extend() {
//        Set<End> res = new HashSet<End>();
//    }
//    /** Select the next end from the fringe and expand the traversal.
//     * @return null when there are no more ends in the fringe */
//    public End extendOne() {
//        for (End next: fringe) {
//            extend(next);
//            return next;
//        }
//        return null;
//    }
//
//    /** Select all the ends in the fringe and expand the traversal. */
//    public Set<End> extendAll() {
//        Set<End> res = new HashSet<End>();
//        res.addAll(fringe);
//        extend(fringe);
//        return res;
//    }


//    public Set<End> getEnds() {
//        return this.ends;
//    }
//    public List<End> getFringe() {
//        return this.fringe;
//    }
}
