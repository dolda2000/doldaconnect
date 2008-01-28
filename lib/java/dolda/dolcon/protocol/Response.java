package dolda.dolcon.protocol;

import java.util.*;

public class Response {
    public List<List<String>> lines;
    public Command cmd;
    public int code;
    
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
    
    public List<String> line(int line) {
	return(lines.get(line));
    }
}
