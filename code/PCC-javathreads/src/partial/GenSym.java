package partial;

/**
 * Created by IntelliJ IDEA.
 * User: dave
 * Date: 11/01/12
 * Time: 15:46
 * To change this template use File | Settings | File Templates.
 */
public class GenSym {
    static private int index = 0;

    static synchronized String gensym() {
        return "sym" + index++;
    }
}
