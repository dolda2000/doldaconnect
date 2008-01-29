package dolda.dolcon;

public class Hub {
    int id, numpeers;
    String fnet, name, gid;
    String state;
    
    public Hub(int id) {
	this.id = id;
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
}
