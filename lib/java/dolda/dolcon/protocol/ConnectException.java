package dolda.dolcon.protocol;

public class ConnectException extends Exception {
    public ConnectException(String msg, Exception cause) {
	super(msg, cause);
    }
}
