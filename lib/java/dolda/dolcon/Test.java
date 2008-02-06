package dolda.dolcon;

import java.util.*;

class Test {
    public static void main(String[] args) throws Exception {
	System.out.print("Password: ");
	PasswordAuth auth = new PasswordAuth(new Scanner(System.in).nextLine());
	long st = System.currentTimeMillis();
	Session sess = new Session(args[0], args[1], auth);
	sess.addHubListener(new HubListener() {
		public void added(Hub h) {
		    h.addListener(new Hub.Listener() {
			    public void chState(Hub h) {
				System.out.println(h.getId() + ": " + h.getState());
			    }
			    
			    public void chNumPeers(Hub h) {
				System.out.println(h.getId() + ": " + h.getNumPeers());
			    }
			    
			    public void chName(Hub h) {
				System.out.println(h.getId() + ": " + h.getName());
			    }
			});
		}
		
		public void removed(Hub h) {
		}
	    }, true);
	/*
	System.out.println(sess.getHubs());
	sess.close();
	System.out.println(System.currentTimeMillis() - st);
	*/
    }
}
