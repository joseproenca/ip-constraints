package partial.connectors;

import partial.*;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.RejectedExecutionException;

/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 12/02/12
 * Time: 17:06
 * To change this template use File | Settings | File Templates.
 */
public class PairwiseAsynchrounous {

    public static int pending = 0;
    public static long start;
    public static Thread thread;
    
    private static SinkEnd[] writers;
    private static int counter;
    
    static private SinkEnd createPair() {
        Writer wrt = new ContinuousWriter(1);
//        {
//            @Override
//            public void updateState() {
//                System.out.println("updating <"+getEnds().get(0).getName()+">");
//                if (limit == 1)
//                    System.out.println("<"+getEnds().get(0).getName()+">");
//                super.updateState();
//            }
//        };
//        OwnerManager.register(wrt.getSink());
        writers[counter]=wrt.getSink();
        counter++;

        Reader rdr = new ContinuousReader(1) {
            @Override
            public void updateState() {
//                System.out.println("updating ["+getEnds().get(0).getName()+"]");
//                System.out.println("["+getEnds().get(0).getName()+"/"+limit+"(before update)]");
                if (limit == 1) {
//                    System.out.println("["+getEnds().get(0).getName()+"]");
                    PairwiseAsynchrounous.pending--;
                    if (PairwiseAsynchrounous.pending == 0) {
                        System.out.print((System.currentTimeMillis() - PairwiseAsynchrounous.start)+", ");
                        if (thread != null) PairwiseAsynchrounous.thread.interrupt();
//                        OwnerManager.kill();
                    }
                }
                super.updateState();
            }
            @Override
            public String toString() {
                return "CReader"+super.toString();
            }

        };

        Replicator r1 = new Replicator();

        wrt.getSink().connect(r1.getSource());
        r1.getSink1().connect(rdr.getSource());

        return r1.getSink2();
    }

    static private SourceEnd addStream(SourceEnd src) {

        SinkEnd snk = createPair();

        Replicator r2 = new Replicator();
        AsyncDrain ad = new AsyncDrain();

        r2.getSource().connect(snk);
        src.connect(r2.getSink1());
        ad.getSource1().connect(r2.getSink2());

        return ad.getSource2();
    }

    static private void createConnector(int n,int threads) {

        writers = new SinkEnd[n];
        counter = 0;

        SinkEnd snk = createPair();
        AsyncDrain ad = new AsyncDrain();        
        snk.connect(ad.getSource1());

        SourceEnd src = ad.getSource2();

        for (int i=2; i<n;i++) {
            src = addStream(src);
        }

        snk = createPair();
        snk.connect(src);

        // MUST separate in a section per thread.
        // it has 2^height writers
//        int size = n;
//        int rest = size % threads;
//        int firstSection = (int) Math.floor(size/threads);
//
//        for (int i=0; i<firstSection; i++) {
//            for (int thread=0; thread < threads; thread++) {
//                int next = i+(thread*(firstSection));
////                System.out.println("registering: "+next);
//                OwnerManager.register(writers[next]);
//            }
//        }
//
//        for (int i=size-rest; i<size; i++) {
//            ContinuousWriter writer = new ContinuousWriter(1);
////            System.out.println("registering: "+i);
//            OwnerManager.register(writers[i]);
//        }


        // NEW APPROACH: first the even, then the odd writers.
        for  (int i=0; i<n; i+=2) {
            ContinuousWriter writer = new ContinuousWriter(1);
//            System.out.println("registering: "+i);
            OwnerManager.register(writers[i]);
        }
        for  (int i=1; i<n; i+=2) {
            ContinuousWriter writer = new ContinuousWriter(1);
//            System.out.println("registering: "+i);
            OwnerManager.register(writers[i]);
        }
    }


    static boolean run(int size,int threads, ExpansionStrategy strategy,long tolerance) {

        OwnerManager.init(threads, strategy);
        createConnector(size,threads);

        PairwiseAsynchrounous.pending = size;
        PairwiseAsynchrounous.start = System.currentTimeMillis();
        PairwiseAsynchrounous.thread = Thread.currentThread();

        try {
            OwnerManager.go();

            Thread.sleep(tolerance);
            thread = null;
            // killed inactive threadpool
            OwnerManager.kill(); // threads in the pool throw RejectedExecutionException when cancelling tasks.
//            System.out.print("-");
//            interruptToContinue.interrupt();
            return false;
        }
        catch (InterruptedException e) {
            // successful execution!
            OwnerManager.kill(); // throws RejectedExecutionException to cancelled tasks.
            System.out.print(".");;
            return true;
        }
        catch (RejectedExecutionException e) {
            System.out.print("** ");
            return false;
        }
    }

    static public void main(String[] args) {

        int size = 5;

//        run(size,1,new LazyExpansionStrategy());     // initate with n thread
//        run(size,2,new LazyExpansionStrategy(),size*200);     // initate with n thread
        run(size,1,new CompleteExpansionStrategy(),1000); // initate with n thread
//        run(size,2,new DefaultExpansionStrategy());  // initate with n thread

//        System.out.println("Main thread ended.");
    }
}
