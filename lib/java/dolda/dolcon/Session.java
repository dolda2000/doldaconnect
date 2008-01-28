package dolda.dolcon;

import java.util.*;
import dolda.dolcon.protocol.*;

public class Session {
    private Connection conn;
    
    public Session(String aspec, String username, List<Authenticator> auth) throws AuthException, ProtocolException, InterruptedException {
	conn = new Connection(aspec);
	conn.expectVersion(2);
	try {
	    conn.syncConnect();
	} catch(ConnectException e) {
	    throw(new ProtocolException(e));
	}
	authenticate(username, auth);
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
    
    public void close() {
	conn.close();
    }
}
