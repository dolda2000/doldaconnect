package dolda.dolcon;

public class VersionException extends Exception {
    public final int r, l, h;
    
    public VersionException(int r, int l, int h) {
	super("Unexpected protocol revision: " + l + "-" + h + ", wanted " + r);
	this.r = r;
	this.l = l;
	this.h = h;
    }
}
