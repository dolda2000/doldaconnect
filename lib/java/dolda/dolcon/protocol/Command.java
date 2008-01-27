package dolda.dolcon.protocol;

import java.util.*;

public class Command {
    List<String> tokens;
    Set<Listener> listeners = new HashSet<Listener>();
    Response resp;
    
    public abstract class Listener {
	public Listener() {
	    addlst(this);
	}
	
	public abstract void done(Response resp) throws Exception;
	public abstract void error(Exception cause);
    }

    public Command(List<String> tokens) {
	this.tokens = tokens;
    }
    
    public Command(String... tokens) {
	this(Arrays.asList(tokens));
    }
    
    private synchronized void addlst(Listener l) {
	listeners.add(l);
    }
    
    public synchronized void done(Response resp) throws Exception {
	this.resp = resp;
	for(Listener l : listeners)
	    l.done(resp);
    }
    
    public synchronized void error(Exception cause) {
	for(Listener l : listeners)
	    l.error(cause);
    }
}
