package dolda.dolcon;

import java.util.*;
import dolda.dolcon.protocol.*;

public class Session implements NotifyListener {
    private Connection conn;
    private String state;
    private Set<HubListener> hubls = new HashSet<HubListener>();
    private boolean listening = false;
    private String[] hubstate = {"none"};
    private String[][] states = {hubstate};
    private Map<Integer, Hub> hubs = new TreeMap<Integer, Hub>();
    
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
		System.out.println(a);
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
    
    private void checkstates() {
	boolean active = false;
	for(String[] sp : states) {
	    if(sp[0] != "none") {
		active = true;
		break;
	    }
	}
	if(listening && !active)
	    conn.removeNotifyListener(this);
	else if(!listening && active)
	    conn.addNotifyListener(this);
    }

    private int atoi(String a) {
	return(Integer.parseInt(a));
    }

    private void fetchhubs() {
	synchronized(hubstate) {
	    if(hubstate[0] != "none")
		return;
	    hubstate[0] = "fetch";
	}
	Command cmd = new Command("lsnodes");
	cmd.new Listener() {
		public void done(Response r) {
		    if(r.code != 200)
			return;
		    for(List<String> line : r.lines) {
			Hub h = new Hub(atoi(line.get(0)));
			h.fnet = line.get(1).intern();
			h.name = line.get(2);
			h.numpeers = atoi(line.get(3));
			h.state = new String[] {"syn", "hs", "est", "dead"}[atoi(line.get(4))];
			h.gid = line.get(5);
			hubs.put(h.id, h);
		    }
		}
		
		public void error(Exception e) {
		}
	    };
	conn.qcmd(new Command("notify fn:act on"), cmd);
    }
    
    public void addHubListener(HubListener hl, boolean addexisting) {
	fetchhubs();
	synchronized(hubls) {
	    hubls.add(hl);
	}
    }
    
    public void removeHubListener(HubListener hl) {
	synchronized(hubls) {
	    hubls.remove(hl);
	    if(hubls.isEmpty()) {
		hubs.clear();
		hubstate[0] = "none";
		checkstates();
	    }
	}
    }

    public void notified(Response resp) {
    }
    
    public void close() {
	conn.close();
	state = "closed";
    }
    
    protected void finalize() {
	if(state != "closed")
	    close();
    }
}
