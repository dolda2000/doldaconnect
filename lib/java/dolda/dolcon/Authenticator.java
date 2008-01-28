package dolda.dolcon;

import dolda.dolcon.protocol.Command;
import dolda.dolcon.protocol.Response;
import java.util.List;

public interface Authenticator {
    public String handles(List<String> name);
    public Command step(Response resp) throws AuthException, ProtocolException, InterruptedException;
}
