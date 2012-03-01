package partial;

import choco.kernel.model.variables.integer.IntegerVariable;

import java.util.*;
import java.util.concurrent.*;

/**
 * Created by IntelliJ IDEA.
 * User:
 * * Date: 10/01/12
 * Time: 14:06
 * To change this template use File | Settings | File Templates.
 */
public class OwnerManager {

    ////////////// DEBUG /////////////////
    static final boolean DEBUG = false;
    static void debugln(String str) {
        if (DEBUG) System.out.println(str);
    }
    static void debug(String str) {
        if (DEBUG) System.out.print(str);
    }
    /////////////////////////////////////


    // 'bindings' manage claimed parts of the connector
    static private Map<Thread, Set<End>> bindings = new HashMap<Thread, Set<End>>();
    static private Map<End, Thread> reverse = new HashMap<End, Thread>();
    // 'registered' manages starting points
    static private Set<End> registered = new HashSet<End>();
    // 'executor' controls the thread pool.
//    static private Executor executor = null;
    static public ThreadPoolExecutor executor = null;
    // 'seed' is a counter for tasks
    static private int seed = 0;
    // 'pending' manages the  startingPoints waiting to be started, to avoid repetition.
    static private Set<End> pending = new HashSet<End>();
    // strategy creator;
    static private ExpansionStrategy strategyFactory;
//    // thieves manage what threads stole from what threads
//    static private Map<Thread,Set<Thread>> thief = new HashMap<Thread, Set<Thread>>();
//    // if a stolen thread got a solution!
//    static private Set<Thread> interruptedSols = new HashSet<Thread>();
    // keeps track of the new traversal of a stolen thread.
    static private Map<Traversal,Set<Thread>> thiefTraversal = new HashMap<Traversal, Set<Thread>>();    
    

    public static synchronized void addInterruptedSol() {
//        interruptedSols.add(Thread.currentThread());
        // inneficcient inverse traversal: it's ok, since it is seldom done,
        // and the map should not be bigger than the number of threads (small map). 
        for (Map.Entry entry: thiefTraversal.entrySet()) {
            for (Thread thread : (Set<Thread>) entry.getValue()) {
                if (thread == Thread.currentThread())
                    ((Traversal) entry.getKey()).setTryToSolve();
                thiefTraversal.remove((Traversal)entry.getKey());
            }
        }
    }


    /**
     * Creates the pool of threads.
     * @param nthreads number of threads to create
     */
    public static void init(int nthreads, ExpansionStrategy strategy) {
        bindings = new HashMap<Thread, Set<End>>();
        reverse = new HashMap<End, Thread>();
        registered = new HashSet<End>();
        seed = 0;
        pending = new HashSet<End>();

        executor = new ThreadPoolExecutor(nthreads, nthreads, 1000, TimeUnit.SECONDS, new LinkedBlockingQueue<Runnable>());
        strategyFactory = strategy;
    }

    /**
     * Shutdowns the executor. 
     */
    public static void kill() {
//        System.out.println("~~~ "+executor.getTaskCount()+"-"+executor.getCompletedTaskCount()+"-"+executor.getActiveCount());
//        for (Runnable r: executor.getQueue()) {
//            System.out.println("~~~ "+r.getClass().getSimpleName()+"-"+r.toString());
//        }
        executor.shutdownNow();
    }

//    /**
//     * Shutdown the existing pool of threads, reset all parameters, and starts again based on the init parameters
//     * @param nthreads first init parameter
//     * @param strategy second init parameter
//     */
//    public static void reset(int nthreads, ExpansionStrategy strategy) {
//        kill();
//
//        bindings = new HashMap<Thread, Set<End>>();
//        reverse = new HashMap<End, Thread>();
//        registered = new HashSet<End>();
//        seed = 0;
//        pending = new HashSet<End>();
//
//        init(nthreads,strategy);
//    }

    /**
     * Creates and executes tasks for each registered & unclaimed & ready end
     */
    public static synchronized void go() {
        for (End end: registered)
            if (!bindings.containsKey(end) && end.isReady() && !pending.contains(end)) {
                executor.execute(new Task(end, strategyFactory.createStrategy(),seed++));
                pending.add(end);
            }
//            else
//                System.out.println("has no binding? is ready? is not pending("+end.getName()+")? " +
//                        !bindings.containsKey(end) + "/" + end.isReady() + "/" + !pending.contains(end));
    }

    
    static public synchronized void start(End end) {
//        System.out.println("Dropping pending task: "+end.getName());
        pending.remove(end);
    }

    /**
     * Claim to get the initial set of ends. Also drops the ends from the 'pending'-pool.
     * @param ends ends to be claimed.
     * @return false if any of the ends is already claimed. 
     */
    static public synchronized boolean claim(Collection<End> ends) {

        boolean free = true;
        boolean someReady = false;
        for (End e : ends) {
            if (reverse.containsKey(e)) {      // TODO: potential problem: this thread owns them already!
                free = false;
                break;
            }
            if (e.isReady())
                someReady = true;
        }

//        if(!someReady) System.out.println("NO END READY! " + ends.iterator().next().getName());
        if (!free || !someReady) return false;

        Thread thread = Thread.currentThread();

        Set<End> endset = new HashSet<End>();
        endset.addAll(ends);
        bindings.put(thread, endset);
        for (End e : ends) {
            reverse.put(e, thread);
        }
        return true;
    }

    /**
     * Grab the primitives attached to a given set of ends, by claiming their ends.
     * @param current state of the traversal (graph and fringe)
     * @param extension selection from the fringe to be extended 
     * @return false if the thread is not registered, or if an end from the current
     * traversal is not owned (claimed) any more.
     */
    static public synchronized boolean 
    extendClaim(Traversal current, Collection<End> extension, Map<String,IntegerVariable> vars) {
        // NOTE: current already contains extension!
        // assert(for (End e: extension) current.ends.contains(e));

        Thread thread = Thread.currentThread();
        // check that current is owned by current thread.            // TODO: see if I can eliminate this test
        boolean allmine = true;

        Set<End> claimed = bindings.get(thread);
        if (claimed != null) {
            for (End e : current.ends) {
                if (!extension.contains(e) && !claimed.contains(e)) {
                    allmine = false;
//                    System.out.println("End is not mine: "+e.getName());
//                    System.out.println("is not extension?/ is not claimed? "+!extension.contains(e)+"/"+!claimed.contains(e));
//                    for (End c: claimed) System.out.println(" - "+c.getName());

                    break;
                }
            }
        }

        if (claimed == null || !allmine) {
            return false;
        }
        
//        // check if a previously stolen thread got a solution
//        if (thief.containsKey(thread)) {
//            for (Thread t: thief.get(thread)) {
//                if (interruptedSols.contains(t)) {
////                    System.out.print("<interrSol!>");
//                    current.setTryToSolve();
//                    interruptedSols.remove(t);
//                    thief.get(thread).remove(t);
//                }
//            }
//            if (thief.get(thread).isEmpty())
//                thief.remove(thread);
//        }
        
           

        // get other threads that own any of the current expansion (future)
        Set<Thread> theOthers = new HashSet<Thread>();
        for (End e : extension) {
            Thread other = reverse.get(e);
            if (other != null && other != thread) {
                theOthers.add(other);
            }
        }

//        if (!theOthers.isEmpty())
//            System.out.println("## MERGING constraints ##");
        /// STEAL ALL GUYS from theOthers and disable them ///

        Set<Primitive> otherPrims = new HashSet<Primitive>();
        for (Thread t : theOthers) {
            
            if (!thiefTraversal.containsKey(current))
                thiefTraversal.put(current,new HashSet<Thread>());
            thiefTraversal.get(current).add(t);
//            System.out.print("<stealing>");
            
            Set<End> theirs = bindings.remove(t);
            if (theirs != null) {
                claimed.addAll(theirs);
                for (End e : theirs) {
                    reverse.remove(e);
                    reverse.put(e,thread);
                    otherPrims.add(e.getPrimitive());
                }
                // ADD ENDS TO THE TRAVERSAL
                theirs.removeAll(current.ends); // remove repeated
                current.ends.addAll(theirs);
                // UPDATE FRINGE (should be the strategy to decide...)
                List<End> theirList = new ArrayList<End>();
                theirList.addAll(theirs);
                strategyFactory.updateFringe(current,theirList);
                // UPDATE INTEGER VARIABLES
            }
            debugln("Interrupting other thread...");
            t.interrupt();
//            newEnds.addAll(theirs); // to added to the expansion!
        }
        claimed.addAll(extension);

        for (Primitive p: otherPrims)
//              vars.putAll(p.getVars()); // overriding? AVOID:
            for (Map.Entry<String,IntegerVariable> entry: p.getVars().entrySet())
                if (!vars.containsKey(entry.getKey()))
                    vars.put(entry.getKey(),entry.getValue());
//            p.renewConstraints();

        bindings.put(thread,claimed);
        for (End e: claimed)
            reverse.put(e,thread);

        return true;
    }

    /**
     * Drops all claims of the current thread (unbinds) and start possible new tasks
     * @param stop do not restart if 'stop'.
     * @param traversal the traversal that is being released (only used for thief management).
    */
    static public synchronized void release(Traversal traversal,  boolean stop) {
//        System.out.println("releasing. continuing? " + !stop);
        Thread thread = Thread.currentThread();
        unbind(thread);
        debug("Pending: ");
        for (End e: pending) debug(e.getName()+", ");
        debugln("");
        
//        if (thief.containsKey(thread)) {
//            for (Thread t: thief.get(thread))
//                interruptedSols.remove(t);
//            thief.remove(thread);
//        }
        thiefTraversal.remove(traversal);

        try{
            if (!stop) go();
        } catch (RejectedExecutionException e) {
//            System.out.print("* ");
        }
    }

    static private void unbind(Thread t) {
        Set<End> mine = bindings.remove(t);

        if (mine != null) {
            for (End e : mine) {
                reverse.remove(e);
            }
        }
    }

    /**
     * Register an end as a starting point.
     * @param end to be registered as starting point.
     */
    public static synchronized void register(End end) {
        registered.add(end);
    }

    
}
