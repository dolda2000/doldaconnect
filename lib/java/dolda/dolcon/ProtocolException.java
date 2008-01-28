package dolda.dolcon;

/**
 * The purpose of this exception is to wrap together all the low-level
 * protocol exceptions, that the programmer is unlikely to want to
 * differentiate between.
 */
public class ProtocolException extends Exception {
    public ProtocolException(String msg) {
	super(msg);
    }
    
    public ProtocolException(Exception cause) {
	super("Unhandled DC protocol condition", cause);
    }
}
