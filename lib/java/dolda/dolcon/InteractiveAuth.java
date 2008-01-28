package dolda.dolcon;

import java.util.List;
import dolda.dolcon.protocol.Response;
import dolda.dolcon.protocol.Command;

public abstract class InteractiveAuth implements Authenticator {
    public String handles(List<String> name) {
	if(name.contains("pam"))
	    return("pam");
	return(null);
    }
    
    public Command step(Response resp) throws AuthException, ProtocolException, InterruptedException {
	if(resp.code == 301) {
	    return(new Command("pass", promptnoecho(resp.token(0, 0))));
	} else if(resp.code == 302) {
	    return(new Command("pass", promptecho(resp.token(0, 0))));
	} else if(resp.code == 303) {
	    info(resp.token(0, 0));
	    return(new Command("pass", ""));
	} else if(resp.code == 304) {
	    error(resp.token(0, 0));
	    return(new Command("pass", ""));
	} else {
	    throw(new ResponseException(resp, 0));
	}
    }
    
    public abstract String promptecho(String msg) throws AuthException;
    public abstract String promptnoecho(String msg) throws AuthException;
    public abstract void info(String msg) throws AuthException;
    public abstract void error(String msg) throws AuthException;
}
