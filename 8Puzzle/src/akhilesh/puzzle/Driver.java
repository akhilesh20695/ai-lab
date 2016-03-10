package akhilesh.puzzle;

public class Driver {

	  public static void main(String args[]){
		  	//Initial Configuration
	        String str="012345678"; // Input the Board State as a String with 0 as the Blank Space  
	        //Change The class name to Implement different Algorithms
	        //EightPuzzle: BFS
	        //EightPuzzleDFS: DFS
	        //EightPuzzleLDFS: Limited DFS
	        
	        EightPuzzleLDFS e = new EightPuzzleLDFS();             // New Instance of the EightPuzzle
	        e.add(str, null);                                                   // Add the Initial State

	        while(!e.agenda.isEmpty()){
	            String currentState = e.agenda.pop();
	            e.up(currentState);                                       // Move the blank space up and add new state to queue
	            e.down(currentState);                                     // Move the blank space down
	            e.left(currentState);                                     // Move left
	            e.right(currentState);                          // Move right and remove the current node from Queue
	        }

	  }
}