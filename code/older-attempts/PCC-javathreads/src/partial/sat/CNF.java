package partial.sat;


import java.util.*;

/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 11/01/12
 * Time: 10:52
 * To change this template use File | Settings | File Templates.
 *
 * @deprecated
 */
public class CNF {
    List<Integer[]> clauses = new ArrayList<Integer[]>();
    HashMap<String, Integer> varnumbers = new HashMap<String, Integer>();
    List<String> varnames = new ArrayList<String>();
    private int seed = 0;

    //def nvars = seed
    //int nclauses = clauses.size;

    public CNF(Integer[][] clauses, String[] vars) {
        Collections.addAll(this.clauses, clauses);
        Collections.addAll(this.varnames, vars);
    }

    private int update(CNF other, int i) {
        int sign;
        if (i < 0) sign = -1;
        else sign = 1;
        int n = sign * i;
        String v = other.varnames.get(n - 1);
        if (!(varnumbers.containsKey(v))) {
            varnames.add(v);
            seed = seed + 1;
            varnumbers.put(v, seed);
        }
        return varnumbers.get(v) * sign;
    }

    public void merge(CNF other) {

        for (Integer[] clause : other.clauses) {
            List<Integer> newclause = new ArrayList<Integer>();
//		for (i <- (clause.size-1) until -1 by -1)
            for (Integer literal : clause) // inverting order, but it's ok
                newclause.add(update(other, literal));
            clauses.add((Integer[]) newclause.toArray());
        }
//        return this;
    }

//    String show() {
//        clauses.map(_.mkString("[",",","] ")).mkString(" ") +
//                "-- " + varname.mkString(",")
//    }

//    void addSomeFlow() {
//            clauses += Array.range(1,seed+1)
//            this
//    }

//    override def toString = {
//        show
//    }

}
