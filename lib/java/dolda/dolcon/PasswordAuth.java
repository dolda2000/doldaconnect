package dolda.dolcon;

import java.util.List;
import dolda.dolcon.protocol.Response;
import dolda.dolcon.protocol.Command;

public class PasswordAuth implements Authenticator {
    private String password;
    
    public PasswordAuth(String password) {
	this.password = password;
    }
    
    public String handles(List<String> name) {
	if(name.contains("pam"))
	    return("pam");
	return(null);
    }
    
    public Command step(Response resp) throws ProtocolException {
	if((password != null) && (resp.code == 301)) {
	    try {
		return(new Command("pass", password));
	    } finally {
		password = null;
	    }
	} else {
	    throw(new ResponseException(resp, 0));
	}
    }
}
