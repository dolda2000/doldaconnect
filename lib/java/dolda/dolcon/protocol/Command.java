package dolda.dolcon.protocol;

import java.util.*;

public class Command {
    List<String> tokens;
    Set<Listener> listeners = new HashSet<Listener>();
    Response resp;
    
    public interface Listener {
	public void done(Response resp) throws Exception;
	public void error(Exception cause);
    }

    public Command(List<String> tokens) {
	this.tokens = tokens;
    }
    
    public Command(String... tokens) {
	this(Arrays.asList(tokens));
    }
    
    public void addListener(Listener l) {
	listeners.add(l);
    }
    
    public void done(Response resp) throws Exception {
	this.resp = resp;
	for(Listener l : listeners)
	    l.done(resp);
    }
    
    public void error(Exception cause) {
	for(Listener l : listeners)
	    l.error(cause);
    }
}
