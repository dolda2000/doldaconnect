package dolda.dolcon;

import dolda.dolcon.protocol.Response;

public class ResponseException extends ProtocolException {
    Response resp;
    int expected;
    
    public ResponseException(Response resp, int expected) {
	super("Unhandled DC protocol response (" + resp.code + " != " + expected + ")");
	this.resp = resp;
	this.expected = expected;
    }
    
    public ResponseException(String msg, Response resp, int expected) {
	super(msg);
	this.resp = resp;
	this.expected = expected;
    }
    
    public static Response check(Response resp, int expect) throws ResponseException {
	if(resp.code != expect)
	    throw(new ResponseException(resp, expect));
	return(resp);
    }
}
