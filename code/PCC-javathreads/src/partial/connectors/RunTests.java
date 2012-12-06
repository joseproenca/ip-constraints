package partial.connectors;

import partial.*;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 14/02/12
 * Time: 17:09
 * To change this template use File | Settings | File Templates.
 */
public class RunTests {

    
    static String label = "";

    static public void main(String[] args) {

        final int tries = 12;

        try {
            DateFormat dateFormat = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss");
            Date date = new Date();
            System.out.println("Date: "+ dateFormat.format(date));
            System.out.println("Nr. of processors: " + Runtime.getRuntime().availableProcessors());

            int mode = 0; // run all

            if (args.length == 1)
                mode = Integer.parseInt(args[0]);


            System.out.println("#### warming up... ####");
            runMergeTree(25, 4,1,new CompleteExpansionStrategy(),1000);

            if (mode == 0) {

                // mergeTree
                System.out.println("## MergeTree ##");
                label = "all";
                for (int i=0; i<=3; i++) {
                  runMergeTree(tries, i,1,new CompleteExpansionStrategy(),1000);
                }
                runMergeTree(tries,4,1,new CompleteExpansionStrategy(),4000);
                runMergeTree(tries,5,1,new CompleteExpansionStrategy(),15000);
                runMergeTree(tries,6,1,new CompleteExpansionStrategy(),50000);
                runMergeTree(tries,7,1,new CompleteExpansionStrategy(),90000);

            }


            if (mode < 2) {
//            System.out.println("#height time (1T/MergeTree)");
            label = "T1";
            for (int i=0; i<=5; i++) {
                runMergeTree(tries, i,1,new LazyExpansionStrategy(),1000);
            }
            runMergeTree(tries, 6,1,new LazyExpansionStrategy(),3000);
            runMergeTree(tries, 7,1,new LazyExpansionStrategy(),15000);
            }

            if (mode < 3) {
//            System.out.println("#height time (2T/MergeTree)");
            label = "T2";
            for (int i=0; i<=3; i++) {
                runMergeTree(tries, i,2,new LazyExpansionStrategy(),500);
            }
            runMergeTree(tries, 4,2,new LazyExpansionStrategy(),3000);
            runMergeTree(tries, 5,2,new LazyExpansionStrategy(),90000);
            runMergeTree(tries, 6,2,new LazyExpansionStrategy(),90000);
            runMergeTree(tries, 7,2,new LazyExpansionStrategy(),120000);
            }

            if (mode < 4) {
            label = "T4";
            for (int i=0; i<=7; i++) {
                runMergeTree(tries, i,4,new LazyExpansionStrategy(),90000);
            }
            }


            if (mode < 5) {
            // PairwiseAsync
            System.out.println("## PairwiseAsync ##");
            label = "all";
            runPairwiseAsync(tries,   2, 1, new CompleteExpansionStrategy(), 1000);
            runPairwiseAsync(tries,  50, 1, new CompleteExpansionStrategy(), 5000);
            runPairwiseAsync(tries, 100, 1, new CompleteExpansionStrategy(), 6000);
            runPairwiseAsync(tries, 150, 1, new CompleteExpansionStrategy(), 10000);
            runPairwiseAsync(tries, 200, 1, new CompleteExpansionStrategy(), 60000);
            runPairwiseAsync(tries, 250, 1, new CompleteExpansionStrategy(), 120000);
//            runPairwiseAsync(tries, 300, 1, new CompleteExpansionStrategy(), 90000);
            }

            if (mode < 6) {
            label = "T1";
            runPairwiseAsync(tries,  2  ,1,new LazyExpansionStrategy(), 1000);
            runPairwiseAsync(tries, 50  ,1,new LazyExpansionStrategy(), 2000);
            runPairwiseAsync(tries,100  ,1,new LazyExpansionStrategy(), 2500);
            runPairwiseAsync(tries,150  ,1,new LazyExpansionStrategy(), 4000);
            runPairwiseAsync(tries,200  ,1,new LazyExpansionStrategy(), 4000);
            runPairwiseAsync(tries,250  ,1,new LazyExpansionStrategy(), 5000);
//            runPairwiseAsync(tries,300  ,1,new LazyExpansionStrategy(), 6000);
//            runPairwiseAsync(tries,400  ,1,new LazyExpansionStrategy(), 7000);
//            runPairwiseAsync(tries,500  ,1,new LazyExpansionStrategy(), 7000);
//            runPairwiseAsync(tries,600  ,1,new LazyExpansionStrategy(), 8000);
//            runPairwiseAsync(tries,700  ,1,new LazyExpansionStrategy(), 8000);
//            runPairwiseAsync(tries,800  ,1,new LazyExpansionStrategy(), 9000);
//            runPairwiseAsync(tries,900  ,1,new LazyExpansionStrategy(), 9000);
//            runPairwiseAsync(tries,1000 ,1,new LazyExpansionStrategy(), 10000);
            }

            if (mode < 7) {
            label = "T2";
            runPairwiseAsync(tries,  2,2,new LazyExpansionStrategy(),  600);
            runPairwiseAsync(tries, 50,2,new LazyExpansionStrategy(), 1000);
            runPairwiseAsync(tries,100,2,new LazyExpansionStrategy(), 1000);
            runPairwiseAsync(tries,150,2,new LazyExpansionStrategy(), 1000);
            runPairwiseAsync(tries,200,2,new LazyExpansionStrategy(), 1000);
            runPairwiseAsync(tries,250,2,new LazyExpansionStrategy(), 1000);
//            runPairwiseAsync(tries,300,2,new LazyExpansionStrategy(), 2000);
//            runPairwiseAsync(tries,400,2,new LazyExpansionStrategy(), 2000);
//            runPairwiseAsync(tries,500,2,new LazyExpansionStrategy(), 3000);
//            runPairwiseAsync(tries,600,2,new LazyExpansionStrategy(), 4000);
//            runPairwiseAsync(tries,700,2,new LazyExpansionStrategy(), 5000);
//            runPairwiseAsync(tries,800,2,new LazyExpansionStrategy(), 5000);
//            runPairwiseAsync(tries,900,2,new LazyExpansionStrategy(), 5000);
//            runPairwiseAsync(tries,1000,2,new LazyExpansionStrategy(),5000);
            }

            if (mode < 8) {
            label = "T4"; // low low low level of success...
            runPairwiseAsync(tries,  2,4,new LazyExpansionStrategy(),  600);
            runPairwiseAsync(tries, 50,4,new LazyExpansionStrategy(), 1000);
            runPairwiseAsync(tries,100,4,new LazyExpansionStrategy(), 1000);
            runPairwiseAsync(tries,150,4,new LazyExpansionStrategy(), 1000);
            runPairwiseAsync(tries,200,4,new LazyExpansionStrategy(), 1000);
            runPairwiseAsync(tries,250,4,new LazyExpansionStrategy(), 1000);
//            runPairwiseAsync(tries,300,4,new LazyExpansionStrategy(), 2000);
//            runPairwiseAsync(tries,400,4,new LazyExpansionStrategy(), 2000);
//            runPairwiseAsync(tries,500,4,new LazyExpansionStrategy(), 3000);
//            runPairwiseAsync(tries,600,4,new LazyExpansionStrategy(), 4000);
//            runPairwiseAsync(tries,700,4,new LazyExpansionStrategy(), 5000);
//            runPairwiseAsync(tries,800,4,new LazyExpansionStrategy(), 5000);
//            runPairwiseAsync(tries,900,4,new LazyExpansionStrategy(), 5000);
//            runPairwiseAsync(tries,1000,4,new LazyExpansionStrategy(),5000);
            }

            System.out.println("## 8 cores: Merge and PairwiseAsync ##");
            if (mode < 9) {
                label = "T8";
                for (int i=0; i<=7; i++) {
                    runMergeTree(tries, i,8,new LazyExpansionStrategy(),220000);
                }
            }

            if (mode < 10) {
                label = "T8";
                runPairwiseAsync(tries,  2,8,new LazyExpansionStrategy(),  600);
                runPairwiseAsync(tries, 50,8,new LazyExpansionStrategy(), 1000);
                runPairwiseAsync(tries,100,8,new LazyExpansionStrategy(), 1000);
                runPairwiseAsync(tries,150,8,new LazyExpansionStrategy(), 1000);
                runPairwiseAsync(tries,200,8,new LazyExpansionStrategy(), 1000);
                runPairwiseAsync(tries,250,8,new LazyExpansionStrategy(), 1000);
            }

            System.out.println("#tests completed");
        } catch (InterruptedException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        }

    }


//            runPairwiseAsync(tries,   2, 1, new CompleteExpansionStrategy(), 1500, 500, 500);
//            runPairwiseAsync(tries,  75, 1, new CompleteExpansionStrategy(), 5000, 4000, 3000);
//            runPairwiseAsync(tries, 150, 1, new CompleteExpansionStrategy(), 10000, 5000, 4000);
//            runPairwiseAsync(tries, 225, 1, new CompleteExpansionStrategy(), 15000, 11000, 10000);
///////            runPairwiseAsync(tries, 300, 1, new CompleteExpansionStrategy(), 50000, 45000, 44000); // HEAP SPACE!


    private static void runMergeTree(int tries, int i, int threads, ExpansionStrategy strat, long t1) throws InterruptedException {
        System.out.print(label+", "+i+", ");
        for (int j=0; j<tries; j++) {
//            System.out.print("a");
            if (!MergeTree.run(i, threads, strat,t1)) tries++;
//            System.out.print("b");
            System.gc();
//            if(j == 0) Thread.sleep(t1); else Thread.sleep(t2);
        }
        System.out.println("");
    }

    private static void runPairwiseAsync(int tries, int i, int threads, ExpansionStrategy strat, long t3) throws InterruptedException {
        System.out.print(label+", "+i+", ");
        for (int j=0; j<tries; j++) {
//            System.out.print("a");
            if (!PairwiseAsynchrounous.run(i, threads, strat, t3)) tries++;
//            System.out.print("b");
            System.gc();
//            if(j == 0) Thread.sleep(t1); else Thread.sleep(t2);
        }
        System.out.println("");
    }

}
