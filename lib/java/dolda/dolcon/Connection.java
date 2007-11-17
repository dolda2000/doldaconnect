package dolda.dolcon;

import java.io.*;
import java.net.Socket;
import java.util.*;

public class Connection {
    Socket s;
    Reader reader;
    LinkedList<Response> resps = new LinkedList<Response>();
    
    public Connection(String aspec) throws ConnectException {
	try {
	    s = new Socket(aspec, 1500);
	} catch(java.net.UnknownHostException e) {
	    throw(new ConnectException("Could not resolve host " + aspec, e));
	} catch(IOException e) {
	    throw(new ConnectException("Could not connect to host " + aspec, e));
	}
	reader = new Reader(s, resps);
	reader.start();
    }
    
    static private class Reader extends Thread {
	Exception error = null;
	Socket s;
	Collection<Response> resps;
	
	public Reader(Socket s, Collection<Response> resps) {
	    this.s = s;
	    this.resps = resps;
	    setDaemon(true);
	}
	
	public void run() {
	    java.io.Reader r;
	    try {
		r = new BufferedReader(new InputStreamReader(s.getInputStream(), "UTF-8"));
	    } catch(IOException e) {
		synchronized(resps) {
		    resps.notifyAll();
		    error = e;
		}
		return;
	    }
	    String state = "start";
	    StringBuilder ct = new StringBuilder();
	    int code = -1;
	    boolean last = true;
	    List<List<String>>lines = new LinkedList<List<String>>();
	    List<String>tokens = new LinkedList<String>();
	    while(true) {
		char c;
		{
		    int i;
		    try {
			if((i = r.read()) < 0) {
			    throw(new IOException("The server closed the connection"));
			}
		    } catch(IOException e) {
			synchronized(resps) {
			    resps.notifyAll();
			    error = e;
			}
			return;
		    }
		    c = (char)i;
		}
		eat: do {
		    if(state == "start") {
			if(c == '\r') {
			    state = "nl";
			} else if(Character.isWhitespace(c)) {
			} else {
			    if(code == -1)
				state = "code";
			    else
				state = "token";
			    continue eat;
			}
		    } else if(state == "nl") {
			if(c == '\n') {
			    if(code == -1) {
				synchronized(resps) {
				    resps.notifyAll();
				    try {
					throw(new IOException("Illegal response code " + code + " from the server"));
				    } catch(IOException e) {
					error = e;
				    }
				}
				return;
			    }
			    lines.add(tokens);
			    tokens = new LinkedList<String>();
			    if(last) {
				synchronized(resps) {
				    resps.add(new Response(code, lines));
				    resps.notifyAll();
				}
				lines = new LinkedList<List<String>>();
			    }
			    state = "start";
			} else {
			    state = "start";
			    continue eat;
			}
		    } else if(state == "code") {
			if((c == '-') || Character.isWhitespace(c)) {
			    last = c != '-';
			    code = Integer.parseInt(ct.toString());
			    ct.setLength(0);
			    state = "start";
			    continue eat;
			} else {
			    ct.append(c);
			}
		    } else if(state == "token") {
			if(Character.isWhitespace(c)) {
			    tokens.add(ct.toString());
			    ct.setLength(0);
			    state = "start";
			    code = -1;
			    continue eat;
			} else if(c == '\\') {
			    state = "bs";
			} else if(c == '"') {
			    state = "cited";
			} else {
			    ct.append(c);
			}
		    } else if(state == "bs") {
			ct.append(c);
			state = "token";
		    } else if(state == "cited") {
			if(c == '\\')
			    state = "cbs";
			else if(c == '"')
			    state = "token";
			else
			    ct.append(c);
		    } else if(state == "cbs") {
			ct.append(c);
			state = "cited";
		    } else {
			throw(new Error("invalid state " + state));
		    }
		    break;
		} while(true);
	    }
	}
    }

    protected void finalize() {
	try {
	    s.close();
	} catch(IOException e) {
	}
	reader.interrupt();
    }

    public static void main(String[] args) throws Exception {
	Connection c = new Connection("pc18");
	while(true) {
	    while(c.resps.size() > 0) {
		System.out.println(c.resps.remove(0));
	    }
	    synchronized(c.resps) {
		c.resps.wait();
	    }
	}
    }
}
