package partial.connectors;


import partial.*;

import java.util.HashSet;
import java.util.concurrent.*;

/**
 * Created by IntelliJ IDEA.
 * User: dave
 * Date: 11/01/12
 * Time: 15:24
 * To change this template use File | Settings | File Templates.
 */
public class Test {
    static public void main(String[] args) {
        // create connector


        LossySync ls1 = new LossySync(),
                ls2 = new LossySync();
        Replicator r1 = new Replicator(),
                r2 = new Replicator(),
                r3 = new Replicator(),
                r4 = new Replicator();
        Merger m = new Merger();
        SyncDrain sd = new SyncDrain();
        ContinuousReader reader1 = new ContinuousReader(1), reader2 = new ContinuousReader(2);
        // reader 2 has local solutions (is tried first)

        ContinuousWriter writer = new ContinuousWriter(4){
            @Override
            public void updateState() {
                System.out.print("data writen ("+limit+")-(");
                super.updateState();
                System.out.println(limit+")");
            }
        };

        SourceEnd lsi1 = ls1.getSource();
        SinkEnd lso1 = ls1.getSink();

        SourceEnd lsi2 = ls2.getSource();
        SinkEnd lso2 = ls2.getSink();

        SourceEnd ra1 = r1.getSource();
        SinkEnd rb1 = r1.getSink1();
        SinkEnd rc1 = r1.getSink2();

        SourceEnd ra2 = r2.getSource();
        SinkEnd rb2 = r2.getSink1();
        SinkEnd rc2 = r2.getSink2();

        SourceEnd ra3 = r3.getSource();
        SinkEnd rb3 = r3.getSink1();
        SinkEnd rc3 = r3.getSink2();

        SourceEnd ra4 = r4.getSource();
        SinkEnd rb4 = r4.getSink2();
        SinkEnd rc4 = r4.getSink1();

        SourceEnd a = m.getSource1();
        SourceEnd b = m.getSource2();
        SinkEnd c = m.getSink();

        SourceEnd l = sd.getSource1();
        SourceEnd r = sd.getSource2();

        SourceEnd rdr1 = reader1.getSource();
        SourceEnd rdr2 = reader2.getSource();

        SinkEnd wtr = writer.getSink();

        rb1.connect(lsi1);
        rc1.connect(ra2);
        rb2.connect(l);
        rc2.connect(lsi2);

        lso1.connect(ra3);

        a.connect(rb3);
        b.connect(rb4);
        c.connect(r);

        lso2.connect(ra4);

        wtr.connect(ra1);
        rdr1.connect(rc3);
        rdr2.connect(rc4);
        
//        writer.showConnector(new HashSet<Primitive>());
//
//        // Register (some of) the proactive parts
//        StartingPoints.register(wtr);
////        StartingPoints.register(rdr1);
//        
//        StartingPoints.go(10,1);
        
        OwnerManager.init(2, new DefaultExpansionStrategy()); // initate with n thread
        OwnerManager.register(wtr);
        OwnerManager.register(rdr1);
        OwnerManager.register(rdr2);

        OwnerManager.go();

        System.out.println("Main thread ended.");
        try {
            Thread.sleep(2000);
        } catch (InterruptedException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        }
        OwnerManager.kill();

    }
}
