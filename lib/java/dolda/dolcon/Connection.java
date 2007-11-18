package dolda.dolcon;

import java.io.*;
import java.net.Socket;
import java.util.*;

public class Connection {
    private Socket s;
    private Reader reader;
    private Writer writer;
    private Queue<Command> queue = new LinkedList<Command>();
    private Queue<Command> pending = new LinkedList<Command>();
    private int reqver = 2, revlo, revhi;
    private String aspec;
    private String state;
    private Set<ConnectListener> connls = new HashSet<ConnectListener>();
    private Exception error;
    
    public interface ConnectListener {
	public void connected() throws Exception;
	public void error(Exception cause);
    }

    public Connection(String aspec) {
	this.aspec = aspec;
	state = "idle";
    }
    
    public void connect() throws ConnectException {
	state = "connecting";
	try {
	    s = new Socket(aspec, 1500);
	} catch(java.net.UnknownHostException e) {
	    throw(new ConnectException("Could not resolve host " + aspec, e));
	} catch(IOException e) {
	    throw(new ConnectException("Could not connect to host " + aspec, e));
	}
	pending = new LinkedList<Command>();
	Command ccmd = new Command(".connect");
	ccmd.addListener(new Command.Listener() {
		public void done(Response resp) throws Exception {
		    try {
			checkver(resp);
		    } catch(VersionException e) {
			error(e);
			throw(e);
		    }
		    synchronized(connls) {
			state = "connected";
			try {
			    for(ConnectListener l : connls)
				l.connected();
			} finally {
			    connls.clear();
			}
		    }
		}
		
		public void error(Exception cause) {
		    synchronized(connls) {
			try {
			    for(ConnectListener l : connls)
				l.error(cause);
			} finally {
			    connls.clear();
			}
		    }
		}
	    });
	pending.offer(ccmd);
	reader = new Reader(s, pending);
	writer = new Writer(s, queue, pending);
	Thread.UncaughtExceptionHandler h = new Thread.UncaughtExceptionHandler() {
		public void uncaughtException(Thread t, Throwable c) {
		    boolean n = false;
		    if(c instanceof StopCondition) {
			StopCondition s = (StopCondition)c;
			n = s.normal;
			c = s.getCause();
		    }
		    Exception e;
		    if(c instanceof Exception)
			e = (Exception)c;
		    else
			e = new Exception(c);
		    if(!n) {
			close();
			error = e;
		    }
		    synchronized(pending) {
			Command cmd;
			while((cmd = pending.poll()) != null) {
			    cmd.error(e);
			}
		    }
		    synchronized(queue) {
			Command cmd;
			while((cmd = queue.poll()) != null) {
			    cmd.error(e);
			}
		    }
		}
	    };
	reader.setUncaughtExceptionHandler(h);
	writer.setUncaughtExceptionHandler(h);
	reader.start();
	writer.start();
    }
    
    private void checkthread() {
	if(Thread.currentThread() == reader)
	    throw(new RuntimeException("Cannot call synchronous method with dispatch thread!"));
    }
        
    public void syncConnect() throws ConnectException, ClosedException, InterruptedException {
	checkthread();
	final boolean[] donep = new boolean[] {false};
	final Exception[] errp = new Exception[] {null};
	ConnectListener l = new ConnectListener() {
		public void connected() {
		    donep[0] = true;
		    synchronized(this) {
			notifyAll();
		    }
		}
		
		public void error(Exception cause) {
		    donep[0] = true;
		    errp[0] = cause;
		    synchronized(this) {
			notifyAll();
		    }
		}
	    };
	addConnectListener(l);
	connect();
	while(!donep[0]) {
	    synchronized(l) {
		l.wait();
	    }
	}
	if(errp[0] != null)
	    throw(new ClosedException(errp[0]));
    }

    public void expectVersion(int reqver) {
	this.reqver = reqver;
    }
    
    private void checkver(Response resp) throws VersionException {
	revlo = Integer.parseInt(resp.token(0, 0));
	revhi = Integer.parseInt(resp.token(0, 1));
	if((reqver < revlo) || (reqver > revhi))
	    throw(new VersionException(reqver, revlo, revhi));
    }

    public Exception join() throws InterruptedException {
	while(reader.isAlive()) {
	    reader.join();
	}
	close();
	return(error);
    }

    public void addConnectListener(ConnectListener l) {
	synchronized(connls) {
	    if((state != "idle") && (state != "connecting"))
		throw(new IllegalStateException("Already connected"));
	    connls.add(l);
	}
    }

    private void qcmd(Command cmd) {
	synchronized(queue) {
	    queue.offer(cmd);
	    queue.notifyAll();
	}
    }
    
    static private class StopCondition extends Error {
	final boolean normal;
	
	public StopCondition(Exception cause, boolean normal) {
	    super(cause);
	    this.normal = normal;
	}
    }
    
    static private class Writer extends Thread {
	Socket s;
	Queue<Command> queue, pending;
	
	public Writer(Socket s, Queue<Command> queue, Queue<Command> pending) {
	    this.s = s;
	    this.queue = queue;
	    this.pending = pending;
	    setDaemon(true);
	}
	
	private String quote(String t) {
	    if(t.length() == 0)
		return("\"\"");
	    StringBuilder sb = new StringBuilder();
	    boolean quote = false;
	    for(int i = 0; i < t.length(); i++) {
		char c = t.charAt(i);
		if(c == '\"') {
		    sb.append("\\\"");
		} else if(Character.isWhitespace(c)) {
		    quote = true;
		    sb.append(c);
		} else {
		    sb.append(c);
		}
	    }
	    if(quote)
		return("\"" + sb.toString() + "\"");
	    else
		return(sb.toString());
	}

	public void run() {
	    try {
		java.io.Writer w = new OutputStreamWriter(s.getOutputStream(), "UTF-8");
		while(true) {
		    Command cmd;
		    try {
			synchronized(pending) {
			    while(pending.size() > 0)
				pending.wait();
			}
			synchronized(queue) {
			    do {
				if((cmd = queue.poll()) != null)
				    break;
				queue.wait();
			    } while(true);
			}
		    } catch(InterruptedException e) {
			throw(new StopCondition(e, true));
		    }
		    StringBuilder out = new StringBuilder();
		    for(String s : cmd.tokens) {
			if(out.length() > 0)
			    out.append(' ');
			out.append(quote(s));
		    }
		    w.write(out.toString());
		}
	    } catch(IOException e) {
		throw(new StopCondition(e, false));
	    }
	}
    }

    static private class Reader extends Thread {
	Socket s;
	Queue<Command> pending;
	
	public Reader(Socket s, Queue<Command> pending) {
	    this.s = s;
	    this.pending = pending;
	}
	
	private void dispatch(Response resp) throws Exception {
	    if(resp.code < 600) {
		synchronized(pending) {
		    resp.cmd = pending.remove();
		    pending.notifyAll();
		}
		resp.cmd.done(resp);
	    }
	}

	public void run() {
	    try {
		java.io.Reader r = new BufferedReader(new InputStreamReader(s.getInputStream(), "UTF-8"));
		String state = "start";
		StringBuilder ct = new StringBuilder();
		int code = -1;
		boolean last = true;
		List<List<String>> lines = new LinkedList<List<String>>();
		List<String> tokens = new LinkedList<String>();
		while(true) {
		    char c;
		    {
			int i;
			try {
			    if((i = r.read()) < 0) {
				throw(new IOException("The server closed the connection"));
			    }
			} catch(java.nio.channels.ClosedByInterruptException e) {
			    throw(new StopCondition(e, true));
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
				if((code < 100) || (code >= 1000)) {
				    throw(new IOException("Illegal response code " + code + " from the server"));
				}
				lines.add(tokens);
				tokens = new LinkedList<String>();
				if(last) {
				    dispatch(new Response(code, lines));
				    lines = new LinkedList<List<String>>();
				}
				code = -1;
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
	    } catch(Exception e) {
		throw(new StopCondition(e, false));
	    }
	}
    }

    public void close() {
	try {
	    s.close();
	} catch(IOException e) {
	}
	reader.interrupt();
	writer.interrupt();
    }

    protected void finalize() {
	close();
    }
}
