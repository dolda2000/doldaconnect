package dolda.dolcon;

import java.util.*;
import dolda.dolcon.protocol.*;

class HubManager implements NotifyListener {
    private Set<HubListener> hubls = new HashSet<HubListener>();
    private Set<HubListener> delayed = new HashSet<HubListener>();
    private Map<Integer, Hub> hubs = new TreeMap<Integer, Hub>();
    private String state = "none";
    private Session sess;
    
    HubManager(Session sess) {
	this.sess = sess;
    }
    
    private int atoi(String a) {
	return(Integer.parseInt(a));
    }

    private void addall(final HubListener ls) {
	for(final Hub hub : hubs.values()) {
	    sess.dispatch(new Runnable() {
		    public void run() {
			ls.added(hub);
		    }
		});
	}
    }
    
    private void fetchhubs() {
	synchronized(this) {
	    if(state != "none")
		return;
	    state = "fetch";
	}
	Command cmd = new Command("lsnodes");
	cmd.new Listener() {
		public void done(Response r) {
		    if(r.code != 200)
			return;
		    for(List<String> line : r.lines) {
			Hub h = new Hub(atoi(line.get(0)), line.get(1));
			h.name = line.get(2);
			h.numpeers = atoi(line.get(3));
			h.state = new String[] {"syn", "hs", "est", "dead"}[atoi(line.get(4))];
			h.gid = line.get(5);
			hubs.put(h.id, h);
		    }
		    synchronized(HubManager.this) {
			state = "";
			HubManager.this.notifyAll();
			for(HubListener ls : delayed) {
			    addall(ls);
			}
		    }
		}
		
		public void error(Exception e) {
		    synchronized(HubManager.this) {
			state = "closed";
		    }
		}
	    };
	sess.conn.qcmd(new Command("notify", "fn:act", "on"), cmd);
	sess.conn.addNotifyListener(this);
    }
    
    public Collection<Hub> gethubs() throws InterruptedException {
	fetchhubs();
	synchronized(this) {
	    while((state != "") && (state != "closed"))
		wait();
	}
	Collection<Hub> ret = new LinkedList<Hub>();
	synchronized(hubs) {
	    for(Hub h : hubs.values())
		ret.add(h.copy());
	}
	return(ret);
    }
    
    public void addls(HubListener hl, boolean addexisting) {
	fetchhubs();
	synchronized(hubls) {
	    hubls.add(hl);
	}
	if(addexisting) {
	    synchronized(this) {
		if(state != "")
		    delayed.add(hl);
		else
		    addall(hl);
	    }
	}
    }
    
    public void rmls(HubListener hl) {
	synchronized(sess) {
	    synchronized(hubls) {
		hubls.remove(hl);
		if(hubls.isEmpty()) {
		    synchronized(hubs) {
			hubs.clear();
		    }
		    state = "closed";
		    sess.conn.removeNotifyListener(this);
		    sess.hm = null;
		}
	    }
	}
    }
    
    public void notified(Response resp) {
	synchronized(this) {
	    if(state != "")
		return;
	}
	if(resp.code == 604) {
	    final Hub h = new Hub(atoi(resp.token(0, 0)), resp.token(0, 1));
	    synchronized(hubs) {
		hubs.put(h.id, h);
	    }
	    sess.dispatch(new Runnable() {
		    public void run() {
			synchronized(hubls) {
			    for(HubListener ls : hubls)
				ls.added(h);
			}
		    }
		});
	} else if(resp.code == 603) {
	    final Hub h;
	    synchronized(hubs) {
		h = hubs.remove(atoi(resp.token(0, 0)));
	    }
	    sess.dispatch(new Runnable() {
		    public void run() {
			synchronized(hubls) {
			    for(HubListener ls : hubls)
				ls.removed(h);
			}
		    }
		});
	} else if(resp.code == 601) {
	    final Hub h;
	    final String state = new String[] {"syn", "hs", "est", "dead"}[atoi(resp.token(0, 1))];
	    synchronized(hubs) {
		h = hubs.get(atoi(resp.token(0, 0)));
	    }
	    h.state = state;
	    sess.dispatch(new Runnable() {
		    public void run() {
			synchronized(h.ls) {
			    for(Hub.Listener ls : h.ls) {
				ls.chState(h);
			    }
			}
		    }
		});
	} else if(resp.code == 602) {
	    final Hub h;
	    final String name = resp.token(0, 1);
	    synchronized(hubs) {
		h = hubs.get(atoi(resp.token(0, 0)));
	    }
	    h.name = name;
	    sess.dispatch(new Runnable() {
		    public void run() {
			synchronized(h.ls) {
			    for(Hub.Listener ls : h.ls) {
				ls.chName(h);
			    }
			}
		    }
		});
	} else if(resp.code == 605) {
	    final Hub h;
	    final int np = atoi(resp.token(0, 1));
	    synchronized(hubs) {
		h = hubs.get(atoi(resp.token(0, 0)));
	    }
	    h.numpeers = np;
	    sess.dispatch(new Runnable() {
		    public void run() {
			synchronized(h.ls) {
			    for(Hub.Listener ls : h.ls) {
				ls.chNumPeers(h);
			    }
			}
		    }
		});
	}
    }
}
