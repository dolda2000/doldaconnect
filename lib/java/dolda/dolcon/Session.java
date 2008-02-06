package dolda.dolcon;

import java.util.*;
import dolda.dolcon.protocol.*;

public class Session {
    Connection conn;
    private String state;
    private boolean listening = false;
    private Dispatcher dispatcher;
    HubManager hm = null;
    
    public Session(String aspec, String username, List<Authenticator> auth) throws AuthException, ProtocolException, InterruptedException {
	state = "connecting";
	conn = new Connection(aspec);
	conn.expectVersion(2);
	try {
	    conn.syncConnect();
	} catch(ConnectException e) {
	    throw(new ProtocolException(e));
	}
	state = "auth";
	authenticate(username, auth);
	state = "";
	dispatcher = new Dispatcher();
	dispatcher.start();
    }
    
    public Session(String aspec, String username, Authenticator... auth) throws AuthException, ProtocolException, InterruptedException {
	this(aspec, username, Arrays.asList(auth));
    }
    
    private void authenticate(String username, List<Authenticator> auth) throws AuthException, ProtocolException, InterruptedException {
	Response resp;
	
	try {
	    resp = ResponseException.check(conn.ecmd("lsauth"), 200);
	    List<String> mechs = new LinkedList<String>();
	    for(List<String> mech : resp.lines)
		mechs.add(mech.get(0).intern());
	    String use = null;
	    Authenticator au = null;
	    for(Authenticator a : auth) {
		use = a.handles(mechs);
		if(use != null) {
		    au = a;
		    break;
		}
	    }
	    if(use == null)
		throw(new NoMechException());
	    resp = conn.ecmd("login", use, username);
	    while(true) {
		if(resp.code == 200) {
		    return;
		} else if((resp.code / 100) == 3) {
		    resp = conn.ecmd(au.step(resp));
		} else if((resp.code / 100) == 5) {
		    throw(new AuthException(resp.token(0, 0)));
		} else {
		    throw(new ResponseException(resp, 0));
		}
	    }
	} catch(ClosedException e) {
	    throw(new ProtocolException(e));
	}
    }
    
    private HubManager gethm() {
	if(hm == null) {
	    hm = new HubManager(this);
	}
	return(hm);
    }
    
    public synchronized void addHubListener(HubListener hl, boolean addexisting) {
	gethm().addls(hl, addexisting);
    }
    
    public synchronized void removeHubListener(HubListener hl) {
	gethm().rmls(hl);
    }
    
    public synchronized Collection<Hub> getHubs() throws InterruptedException {
	return(gethm().gethubs());
    }
    
    public void close() {
	conn.close();
	state = "closed";
    }
    
    protected void finalize() {
	if(state != "closed")
	    close();
	dispatcher.interrupt();
    }
    
    void dispatch(Runnable ev) {
	dispatcher.dispatch(ev);
    }

    private static class Dispatcher extends Thread {
	private Queue<Runnable> q = new LinkedList<Runnable>();
	
	private Dispatcher() {
	    setDaemon(true);
	}
	
	public void dispatch(Runnable ev) {
	    synchronized(q) {
		q.offer(ev);
		q.notifyAll();
	    }
	}
	
	public void run() {
	    while(true) {
		try {
		    Runnable r;
		    synchronized(q) {
			while((r = q.poll()) == null)
			    q.wait();
		    }
		    r.run();
		} catch(Throwable t) {
		    t.printStackTrace();
		}
	    }
	}
    }
}
