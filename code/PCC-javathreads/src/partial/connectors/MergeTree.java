package partial.connectors;

import choco.kernel.model.constraints.Constraint;
import partial.*;

import java.util.*;
import java.util.concurrent.RejectedExecutionException;

import static partial.sat.Solver.notvar;

/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 10/02/12
 * Time: 13:12
 * To change this template use File | Settings | File Templates.
 */
public class MergeTree {

    public static Thread thread;
    
    static private List<SourceEnd> addMerges(List<SourceEnd> srcs) {
        List<SourceEnd> res = new ArrayList<SourceEnd>();
        for (SourceEnd src:srcs) {
            Merger m = new Merger();
            SourceEnd a = m.getSource1();
            SourceEnd b = m.getSource2();
            res.add(a); res.add(b);
            
            SinkEnd c = m.getSink(); 
            c.connect(src);
        }
        return res;
    }
    
    static private List<SourceEnd> createMergeTree(int n, SourceEnd src) {
        List<SourceEnd> res = new ArrayList<SourceEnd>();
        res.add(src);
        for (int i=0; i<n; i++)
            res = addMerges(res);
        return res;
    }


    static boolean run(int height,int threads, ExpansionStrategy strategy, long tolerance) {

        OwnerManager.init(threads,strategy);
        MergeTree.thread = Thread.currentThread();

        ContinuousReader reader = new ContinuousReader((int) Math.pow(2,height)) {
            long now = System.currentTimeMillis();

            @Override
            public void updateState() {
                if (limit == 1) {
                    System.out.print((System.currentTimeMillis()-now)+", ");
                    if (MergeTree.thread != null) MergeTree.thread.interrupt();
//                    interruptToContinue.interrupt();
                }
                super.updateState();
            }
        };

        List<SourceEnd> srcs = createMergeTree(height,reader.getSource());

        // Divide into threads*2 sections... to avoid more contention.
        int partitions = threads*2;

        // MUST separate in a section per thread.
        // it has 2^height writers
        int size = (int) Math.pow(2,height);
        int rest = size % partitions;
        int sectionSize = (int) Math.floor(size/partitions);

        for (int i=0; i<sectionSize; i++) {
            for (int section=0; section < partitions; section++) {
                ContinuousWriter writer = new ContinuousWriter(1);
                int next = i+(section*(sectionSize));
                writer.getSink().connect(srcs.get(next));
                OwnerManager.register(writer.getSink());
            }
        }

        for (int i=size-rest; i<size; i++) {
            ContinuousWriter writer = new ContinuousWriter(1);
            writer.getSink().connect(srcs.get(i));
            OwnerManager.register(writer.getSink());
        }


//        for (SourceEnd src: srcs) {
//            ContinuousWriter writer = new ContinuousWriter(1);
//            writer.getSink().connect(src);
//            OwnerManager.register(writer.getSink());
//        }

        try {
            OwnerManager.go();

            Thread.sleep(tolerance);
            MergeTree.thread = null;

            // kill inactive threadpool
            OwnerManager.kill(); // threads in the pool throw RejectedExecutionException when cancelling tasks.
            System.out.print(".");
            return false;
        }
        catch (InterruptedException e) {
            // successful execution!
            OwnerManager.kill(); // throws RejectedExecutionException to cancelled tasks.
//            System.out.print("<>");
            return true;
        }
        catch (RejectedExecutionException e) {
            System.out.print("** ");
            return false;
        }
    }

    static public void main(String[] args) {
        
        int height = 7;

        run(height,1,new LazyExpansionStrategy(),50000);     // initate with n thread
//        run(height,2,new LazyExpansionStrategy());     // initate with n thread
//        run(height,1,new CompleteExpansionStrategy()); // initate with n thread
//        run(height,2,new DefaultExpansionStrategy());  // initate with n thread

//        System.out.println("Main thread ended.");
    }
}