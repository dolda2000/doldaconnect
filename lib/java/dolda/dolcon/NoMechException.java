package dolda.dolcon;

public class NoMechException extends AuthException {
    public NoMechException() {
	super("No supported authentication mechanism was offered by the server");
    }
}
