package dolda.dolcon;

public class ClosedException extends Exception {
    public ClosedException(Throwable cause) {
	super("DC connection has been closed", cause);
    }

    public ClosedException() {
	this(null);
    }
}
