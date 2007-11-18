package dolda.dolcon;

import java.util.*;

public class Response {
    List<List<String>> lines;
    Command cmd;
    int code;
    
    public Response(int code, List<List<String>> lines) {
	this.code = code;
	this.lines = lines;
    }
    
    public String toString() {
	return("Response " + code + ": " + lines.toString());
    }
    
    public String token(int line, int token) {
	return(lines.get(line).get(token));
    }
}
