package dolda.dolcon;

import java.util.*;

public class Response {
    List<List<String>>lines;
    int code;
    
    public Response(int code, List<List<String>>lines) {
	this.code = code;
	this.lines = lines;
    }
    
    public String toString() {
	return("Response " + code + ": " + lines.toString());
    }
}
