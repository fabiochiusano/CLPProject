object Pascal {
	def main(): Unit = {
		println(new PascalTriangleMaker().Initialize(15).Print());
	}
}

class PascalTriangleMaker {
	var triangle: Int[];
	
	def Initialize(rows: Int): PascalTriangleMaker = {
		var i: Int;
		var curRow: Int;
		var curCol: Int;
		var n: Int;
		
		n = rows*(rows+1)/2;
		triangle = new Int[n];
		i = 0;
		curRow = 0;
		while(i < n){
			curCol = 0;
			while((curCol < curRow || curCol == curRow) && i < n){
				triangle[i] = this.GetPascal(curRow, curCol);
				i = i + 1;
				curCol = curCol + 1;
			}
			curRow = curRow + 1;
		}
		
		return this;
	}
	
	def GetPascal(row: Int, col: Int): Int = {
		var res: Int;
		
		if(col == 0 || col == row) 
			res = 1;
		else 
			res = this.GetPascal(row-1, col) + this.GetPascal(row-1,col-1);
		
		return res;
	}
	
	def Print(): String = {
		var i: Int;
		var curRow: Int;
		var curCol: Int;
		var str: String;
		var n: Int;
		
		println("");
		println("Pascal Triangle Generator - @Copyright 2015 Fabio&Tommy");
		println("");
		
		i = 0;
		curRow = 0;
		n = triangle.length;
		while(i < n){
			curCol = 0;
			str = "";
			while((curCol < curRow || curCol == curRow) && i < n){
				str = str + triangle[i] + " ";
				i = i + 1;
				curCol = curCol + 1;
			}
			println(str);
			curRow = curRow + 1;
		}
		
		println("");
		return "Fine";
	}
}