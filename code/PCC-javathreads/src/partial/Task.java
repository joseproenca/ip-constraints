package partial;

import choco.Choco;
import choco.kernel.model.constraints.Constraint;
import choco.kernel.model.variables.integer.IntegerVariable;
import partial.sat.Solver;

import java.util.*;

/**
 * Created by IntelliJ IDEA.
 * User: dave
 * Date: 10/01/12
 * Time: 14:50
 * To change this template use File | Settings | File Templates.
 */
public class Task implements Runnable {

    ////////////// DEBUG /////////////////
    static final boolean DEBUG = false;
    static void debugln(String str) {
        if (DEBUG) System.out.println(str);
    }
    static void debug(String str) {
        if (DEBUG) System.out.print(str);
    }
    /////////////////////////////////////

    private Traversal traversal;
    private ExpansionStrategy strategy;
//    private int rounds_to_go;
    private End start;
    private int uid;
    private String item;

    public Task(End end, ExpansionStrategy strategy) {
        this(end,strategy,-1);
    }

    public Task(End startPoint, ExpansionStrategy strategy, int id) {
//        rounds_to_go = rounds;
        this.strategy = strategy;
        this.start = startPoint;
        uid = id;
        item = "* "+id+" > ";
    }

    public void run() {
        OwnerManager.start(start);
        debugln("###############");
        boolean solved = false;
        boolean failStart = false;
        boolean interrupted = false;
        Solver s = null;

        try {
            if (start == null) {
                // will inform thread pool that we have nothing to do
                // this will be caught by the finally clause
//                System.out.println(item+"Unknown starting point. Nothing to do this round.");
                return;
            }

            this.traversal = new Traversal();

            traversal.ends.add(start);
            //update fringe
            traversal.fringe.add(start);

            int count = 0;
//            debugln(item+"Started Task");

            if (!OwnerManager.claim(strategy.initial(traversal))) {
//                System.out.println(item+" Failed claim: couldn't find any work do to ... ");

                failStart = true;
                return; // Couldn't find any work to do. Clean up
            }


            while (true) {
                debugln("-----------------------------");
                debugln(item+"Loop " + count++);
                debugln(traversal.toString());

                Map<String, IntegerVariable> vars = new HashMap<String, IntegerVariable>();

                if (strategy.readyToSolve(traversal)) {
                    debugln(item + "Generating constraints... ");
                    Set<Constraint> constraints;

                    constraints = generateConstraints(vars);
//                    for(String str: start.getPrimitive().getVars().keySet())
//                        debug("--- "+str);
//                    debugln("vars-length: "+vars.size());

                    s = new Solver();
                    solved = s.solve(constraints);
                    debugln(solved ? item+"++Found one++" : item+"--No solution--");
                    if (!solved)     {
                        debugln("traversal again:\n"+traversal.toString());
                        debugln("constraints: \n"+s.showConstraints());
                    }
                }

                synchronized (OwnerManager.class) {

                    if (Thread.interrupted()) {
                       throw new InterruptedException();
                    }


                    if (solved) {
                        debugln("#####################################");
                        debugln(item+"Processing found solution ... *");
                        debugln("#####################################");

                        // TODO: update state based on the solution. Right now triggers updates on all primitives with flow.
                        // update all primitives with flow (release from manager comes later)
                        HashSet<Primitive> primitives = new HashSet<Primitive>();

//                        for (End e: traversal.ends)
//                            //if (s.check(e.getPrimitive().getVars().get(e.getName())))
//                            if (s.check(vars.get(e.getName())))
//                                primitives.add(e.getPrimitive());
                        for (End e: traversal.ends) {
                            debug(e.getName()+"? ");
                            //if (s.check(e.getPrimitive().getVars().get(e.getName())))
                            if (vars.containsKey(e.getName())) {
                                if (s.check(vars.get(e.getName())))
                                    primitives.add(e.getPrimitive());
                                debugln(""+s.check(vars.get(e.getName())));
                            }
                            else debugln("Strange... end not in vars: "+e.getName());
                        }

                        for (Primitive p: primitives){
                            debugln("updating "+p);
                            p.updateState();
                        }
                        return;    // will call clean up routines
                    }
                }

                if (!solved) {
                    List<End> next = strategy.expand(traversal);  // get more ends.
                    if (next.isEmpty()) {
                        //                        System.out.println(item+"End of traversal without solutions... ");
                        return;  // goes to clean up
                    }
//                    Set<End> newEnds = new HashSet<End>();
                    if (!OwnerManager.extendClaim(traversal, next, vars)) {
                        //                        System.out.println(item+"Couldn't extend my claim ... "+traversal.toString());
                        //                        for (End n:next) System.out.print(" "+n.getName());
                        //                        System.out.println();
                        return;  // goes to clean up
                    }
                    continue;  // go back to start of loop
                }
            }
            // TODO:  Need to deal with the case that there is no solution, but all primitives have been considered.
        } catch (InterruptedException e) {    // exceptions originate from claiming and from Thread.interrupted().
            debugln("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%");
            debugln(item+"Interrupted. Clearing out all my stuffs. *");
            debugln("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%");
            interrupted = true;
            //if (solved) OwnerManager.addInterruptedSol();
        } finally {
            traversal = null;
            // stop if initial claim failed or if it was not solved and not interrupted
            debugln("stopping? " + (failStart || (!solved && !interrupted)));
            debugln("failStart/solved/interrupted - "+failStart+"/"+solved+"/"+interrupted);
            OwnerManager.release(traversal,failStart || (!solved && !interrupted));
//                    OwnerManager.release(!solved); // stop if not solved
        }
    }

    /**
     * Collect constraints from the primitives in the traversal.
     * @post refresh all the IntegerVariables for the solver, and keeps a copy in the vars argument.
     * @param vars used to include all new IntegerVariables
     * @return Constraints from the traversal
     */
    private Set<Constraint> generateConstraints(Map<String, IntegerVariable> vars) {
        // Determine constraints for connector
        // determine boundary constraints
        // determine internal flow constraints
        Set<Constraint> constraints = new HashSet<Constraint>(); // set to avoid repetition
        Set<Primitive> primitives = new HashSet<Primitive>();
//        Set<IntegerVariable> vars = new HashSet<IntegerVariable>();
        debug(item+"Border constraints: ");


        // Step 1: renew all constraints of all primitives and get their constraints
        for (End e: traversal.ends)
            primitives.add(e.getPrimitive());

        synchronized (OwnerManager.class) {
            for (Primitive p : primitives) {
                p.renewConstraints();
                constraints.addAll(Arrays.asList(p.getConstraints()));
            }


            // Step 2: collect all noFlowConstraints (border) and flowConstraint (plugging)
            for (End e : traversal.ends) {
                // constraints from primitives + from connected ends + from border + enforce flow.
                if (traversal.fringe.contains(e)) {
                    constraints.add(e.getNoFlowConstraint()); // MUST BE *AFTER* RENEWCONSTRAINTS
                    debug(e.getNoFlowConstraint().pretty()+", ");
                }
                else
                    constraints.add(e.getConstraint());
            }
            debugln("");


            // Step 3: collect all constraints of all primitives, and their VARS mapping
            // Only now the "vars" mapping is complete!
            for(Primitive p: primitives) {
                vars.putAll(p.getVars()); //  addAll(p.getVars().values());
            }
        }

        Constraint someFlow = Choco.FALSE;
        for (IntegerVariable v : vars.values())
            someFlow = Choco.or(Choco.eq(v, 1), someFlow);
        constraints.add(someFlow);
        return constraints;
    }
}

/*
TODO: overall running cycle
aim at:
- collect constraints for primitives P
- try to solve constraints
- if no solution, expand P.
- if solution, lock(owner manager) { update P }

steps missing:
- making it so that I can create channels and plug them together
- bringing in Choco and generating constraints for each primitive.
- provide facilities for traversing a connector to
(1) compute the set of internal nodes based on a given set of Primitives -- this is the set of primitives a
thread owns, which may only be a subset of the primitives held by the connector.
(2) compute the boundary nodes based on a given set of primitives
*/