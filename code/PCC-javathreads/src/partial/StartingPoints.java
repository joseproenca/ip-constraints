package partial;

import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.Executor;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * Created by IntelliJ IDEA.
 * User: dave
 * Date: 22/01/12
 * Time: 11:32
 * To change this template use File | Settings | File Templates.
 */
@Deprecated
public class StartingPoints{
    static private Set<End> startingPoints = new HashSet<End>();

    static synchronized End getStartingPoint() {
        End selected = null;
        for (End e : startingPoints) {
            selected = e;
            startingPoints.remove(e);
            break;
        }
        return selected;
    }
    
    static synchronized void returnStartingPoint(End end) {
        startingPoints.add(end);
    }
    
    static synchronized void register(End end) {
        startingPoints.add(end);
    }

    // %%%%%%

    static private Executor executor = null;

    static void go(int rounds, int threads) {
        executor = new ThreadPoolExecutor(threads, threads, 1000, TimeUnit.SECONDS, new LinkedBlockingQueue<Runnable>());
//        for (int i = 0; i < threads; i++) {
//          executor.execute(new Task(new DefaultExpansionStrategy(),i+1));
//        }
    }

    static void restart(Task finishing) {
        executor.execute(finishing);
    }
}
