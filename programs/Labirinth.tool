object Labirinth { 
	def main ( ) : Unit = { 
		println("Starting Program");
		println("");			
		//*****Initialize(Width, Height, Rooms, Corridors, Seed)*****//
		//*****CreateRooms(MinWidth, MinHeight, MaxTry)*****//
		println(new MakeLab().Initialize(75,75,50,4,12345).CreateRooms(5,5,100).Print());
		//*****Works well up to 200 min 3x3 rooms in a 75x75 space, with 100 as a value for maxTry*****//
		//*****
		//*****Default settings: println(new MakeLab().Initialize(50,50,25,3,12345).CreateRooms(4,4,100).Print());
		//*****					 println(new MakeLab().Initialize(75,75,50,4,12345).CreateRooms(5,5,100).Print());
		//*****
		println("");
		println("Program Ended");
	} 
}

class MakeLab {
	var width:Int;
	var height:Int;
	var roomNumber:Int;
	var corridorsPerRoom:Int;
	
	var corridorTile:String;
	var wallTile:String;
	
	var lab:Int[]; //1 for walls, 0 for corridors or rooms
	
	var util:Util;
	var random:PseudoRandomNumberGenerator;
	
	def Initialize(w:Int,h:Int,roomNum:Int,corrPerRoom:Int,randomSeed:Int):MakeLab = {
		var i:Int;
		
		width = w;
		height = h;
		roomNumber = roomNum;
		corridorsPerRoom = corrPerRoom;
		
		corridorTile = " ";
		wallTile = "#";
		
		util = new Util();
		random = new PseudoRandomNumberGenerator().init(randomSeed);
		
		lab = new Int[w*h];
		
		i = 0;
		while(i < lab.length){
			lab[i] = 1;
			i = i+1;
		}
		
		return this;
	}
	
	def Print():String = {
		var thisRow:String;			
		var i:Int;
		var j:Int;
		
		i = 0;
		
		println("");
		j = 0;
		thisRow = "";
		while(j < width+2){
			thisRow = thisRow + wallTile;
			j = j+1;
		}
		println(thisRow);	
			
		while(i < height){
			j = 0;
			thisRow = "";
			while(j < width){
				if(lab[i*width+j] == 1) //append a wall
					thisRow = thisRow + wallTile;	//"(" + i + "," + j + ")";
				else //append a corridor
					thisRow = thisRow + corridorTile;	//"( , )";
				j = j+1;
			}
			println(wallTile + thisRow + wallTile);
			i = i+1;
		}
		
		j = 0;
		thisRow = "";
		while(j < width+2){
			thisRow = thisRow + wallTile;
			j = j+1;
		}
		println(thisRow);	
		println("");
		
		return "Done";
	}
	
	def CreateRooms(minW:Int, minH:Int, maxTry:Int):MakeLab = {	
		var roomBool:Bool;
	
		while(0 < roomNumber){
			roomBool = this.MakeRoom(minW, minH, maxTry, corridorsPerRoom);
			roomNumber = roomNumber - 1;
		}
		return this;
	}
	
	def MakeRoom(minW:Int, minH:Int, maxTry:Int, corrNum:Int):Bool = {
		var top:Int;
		var left:Int;
		var w:Int;
		var h:Int;
		var maxW:Int;
		var maxH:Int;
		var i:Int;
		var j:Int;
		var pos:Int;
		var overlap:Bool;
		var corrBool:Bool;
		var minusOne:Int; minusOne = 0-1;
		
		top = random.getInt(1,height-2);
		left = random.getInt(1,width-2);
		maxW = (width-left)/4 + minW;
		maxH = (height-top)/4 + minH;
		w = random.getInt(minW,maxW);
		h = random.getInt(minH,maxH);
		overlap = this.RoomOverlap(top,left,w,h);
		while(overlap == true && 0 < maxTry){
			if(minW+1 < maxW)
				maxW = maxW-1;
			if(minH+1 < maxH)
				maxH = maxH-1;
		
			top = random.getInt(1,height-2);
			left = random.getInt(1,width-2);
			w = random.getInt(minW,maxW);
			h = random.getInt(minH,maxH);
			overlap = this.RoomOverlap(top,left,w,h);
			maxTry = maxTry - 1;
		}
		
		i = top;
		while(i < top+h){
			j = left;
			while(j < left+w){
				pos = util.PointToArray(j,i,width,height);
				if(!(pos == minusOne)){
					lab[pos] = 0;
				}
				j = j+1;
			}
			i = i+1;
		}
		
		println("Room Built in " + top + " " + left + " Dimensions: " + w + "x" + h);
		
		while(0 < corrNum){
			println("Starting corridor in: " + top+"+"+h/2 + " " + left+"+"+w/2);
			corrBool = this.MakeCorridor(top+h/2, left+w/2, width, height);
			corrNum = corrNum-1;
		}
		
		return true;
	}
	
	def RoomOverlap(top:Int,left:Int,w:Int,h:Int):Bool = {
		var i:Int;
		var j:Int;
		var flag:Bool;
		var pos:Int;
		var minusOne:Int; minusOne = 0-1;
		
		flag = false;
		i = top;
		if(1 < i)
			i = i-1;
		while(i < top+h+1){
			j = left;
			if(1 < j)
				j = j-1;
			while(j < left+w+1){
				pos = util.PointToArray(j,i,width,height);
				if(!(pos == minusOne) && lab[pos] == 0)
					flag = true;
				if(pos == minusOne)
					flag = true;
				j = j+1;
			}
			i = i+1;
		}
		return flag;
	}
	
	def MakeCorridor(x:Int, y:Int, width:Int, height:Int):Bool = {
		var randomDirection:Int;
		var pos:Int;
		var minusOne:Int; minusOne = 0-1;
		
		randomDirection = random.getInt(0,100);
		
		if(randomDirection < 25){ //corridor to the left
			pos = util.PointToArray(x,y,width,height);
			while(1 < x && !(pos == minusOne) && lab[pos] == 0){ //Move to the end to the room
				x = x-1;
				pos = util.PointToArray(x,y,width,height);
			}
			while(1 < x && !(pos == minusOne) && lab[pos] == 1){ //Create a corridor till another room
				x = x-1;
				lab[pos] = 0;
				pos = util.PointToArray(x,y,width,height);
			}
			println("Corridor to the left done");
		}
		
		else if(randomDirection < 50){ //corridor to the right
			pos = util.PointToArray(x,y,width,height);
			while(x < width-1 && !(pos == minusOne) && lab[pos] == 0){ //Move to the end to the room
				x = x+1;
				pos = util.PointToArray(x,y,width,height);
			}
			while(x < width-1 && !(pos == minusOne) && lab[pos] == 1){ //Create a corridor till another room
				x = x+1;
				lab[pos] = 0;
				pos = util.PointToArray(x,y,width,height);
			}
			println("Corridor to the bottom done");
		}
		
		else if(randomDirection < 75){ //corridor to the top
			pos = util.PointToArray(x,y,width,height);
			while(1 < y && !(pos == minusOne) && lab[pos] == 0){ //Move to the end to the room
				y = y-1;
				pos = util.PointToArray(x,y,width,height);
			}
			while(1 < y && !(pos == minusOne) && lab[pos] == 1){ //Create a corridor till another room
				y = y-1;
				lab[pos] = 0;
				pos = util.PointToArray(x,y,width,height);
			}
			println("Corridor to the top done");
		}
		
		else if(randomDirection < 100){ //corridor to the bottom
			pos = util.PointToArray(x,y,width,height);
			while(y < height-1 && !(pos == minusOne) && lab[pos] == 0){ //Move to the end to the room
				y = y+1;
				pos = util.PointToArray(x,y,width,height);
			}
			while(y < height-1 && !(pos == minusOne) && lab[pos] == 1){ //Create a corridor till another room
				y = y+1;
				lab[pos] = 0;
				pos = util.PointToArray(x,y,width,height);
			}
			println("Corridor to the bottom done");
		}
		
		return true;
	}
}

class Util{
	
	def PointToArray(x:Int,y:Int,width:Int,height:Int):Int = {
		var res:Int;
		if(x < width && y < height)
			res = y * width + x;
		else
			res = 0 - 1;
		return res;
	}
}

class PseudoRandomNumberGenerator {
  var a : Int;
  var b : Int;
 
  def init(x:Int) : PseudoRandomNumberGenerator = {
    a = 123456789 / x; // put whatever you like in here
    b = 987654321 / x; 
    return this;
  }
 
  def getInt(min : Int, max : Int) : Int = {
    var posInt : Int;
 
    posInt = this.nextInt();
    if(posInt < 0)
      posInt = 0 - posInt;
	
	if(min == max)
		max = max + 1; //disallow zero-division errors
	
    return min + (this.mod(posInt, max - min));
  }
 
  def mod(i : Int, j : Int) : Int = { return i - (i / j * j); }
 
  def nextInt() : Int = {
    b = 36969 * ((b * 65536) / 65536) + (b / 65536);
    a = 18000 * ((a * 65536) / 65536) + (a / 65536);
    return (b * 65536) + a;
  }
}