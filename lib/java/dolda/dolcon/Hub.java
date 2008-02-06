package dolda.dolcon;

import java.util.*;

public class Hub {
    int id, numpeers = 0;
    final String fnet;
    String name = "", gid = "";
    String state = "syn";
    Set<Listener> ls = new HashSet<Listener>();
    
    public Hub(int id, String fnet) {
	this.id = id;
	this.fnet = fnet.intern();
    }
    
    public interface Listener {
	public void chName(Hub hub);
	public void chNumPeers(Hub hub);
	public void chState(Hub hub);
    }
    
    public Hub copy() {
	Hub ret = new Hub(id, fnet);
	ret.numpeers = numpeers;
	ret.gid = gid;
	ret.state = state;
	ret.name = name;
	return(ret);
    }
    
    public int getId() {
	return(id);
    }
    
    public String getGid() {
	return(gid);
    }
    
    public String getFnet() {
	return(fnet);
    }
    
    public String getName() {
	return(name);
    }
    
    public int getNumPeers() {
	return(numpeers);
    }
    
    public String getState() {
	return(state);
    }
    
    public void addListener(Listener ls) {
	synchronized(this.ls) {
	    this.ls.add(ls);
	}
    }
    
    public String toString() {
	return("Hub (" + id + ", " + fnet + ", \"" + name + "\")");
    }
}
