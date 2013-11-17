import java.util.Random
import scala.io.Source._
import scala.collection.mutable.Set
//import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer

//The L Game is a simple game for two players played on a 4 by 4 board.
//Each player has an L shaped piece and there are two number single square pieces.
//This code includes classes to represent pieces and board positions and a set to represent the collection of possible positions up to reflection and rotation.

object LGame{
  val lShape = List(List(1,0,0),
	            List(1,0,0),
	            List(1,1,0))
  //A reflection of a square matrix
  def ref(matrix : List[List[Int]]) : List[List[Int]] = {
    val size=matrix.size
    try{
	  for(i <- List.range(0, size)) yield
	    for(j <- List.range(0, size)) yield matrix(size-i-1)(j)
	} catch {
	  case e: Exception => println("Error: Cannot reflect - The array given wasn't square"); throw e;
	}
  }
  //A rotation of a square matrix
  def rot(matrix : List[List[Int]]) : List[List[Int]] = {
    val size=matrix.size
	try{
	  for(i <- List.range(0, size)) yield
	    for(j <- List.range(0, size)) yield matrix(size-j-1)(i)
	} catch {
	  case e: Exception => println("Error: Cannot rotate - The array given wasn't square"); throw e;
	}
  }
  //Symmetries under 90 degree rotations and reflections
  val symmetry : List[List[List[Int]]=>List[List[Int]]] = 
                    List(x=>x, x=> rot(x), x=>rot(rot(x)), x=>rot(rot(rot(x))),
	                x=>ref(x),x=>ref(rot(x)), x=>ref(rot(rot(x))), x=>ref(rot(rot(rot(x)))))
  def applySymmetry(o : Int, matrix : List[List[Int]]) : List[List[Int]]= {
    symmetry(o)(matrix)
  }
  //The possible positions that an L shaped piece can have are represented by an LPosition
  case class LPosition(val offsetX : Int, val offsetY : Int, val orientation : Int){
    assert((orientation> -1)&&(orientation<8), {println("orientation must be between 0 and 7 inclusive")})
	assert((offsetX> -2)&&(offsetX<3),println("Error constructing LPosition: offsetX must be between -1 and 2 inclusive"))
	assert((offsetY> -2)&&(offsetY<3),println("Error constructing LPosition: offsetY must be between -1 and 2 inclusive"))
	override def hashCode : Int = offsetX+4*offsetY+16*orientation
	val shape = applySymmetry(orientation, lShape)
	val isValid : Boolean = {
	  val oX=offsetX; val oY=offsetY
	  val temp = for(x <- List.range(0, 3); y <- List.range(0, 3)) yield
	    ((oX+x== -1)||(oX+x==4)||(oY+y== -1)||(oY+y==4))&&shape(x)(y)==1
	  !temp.exists(i => i)
	}
	def displayShape() = {
	  for(i <- List.range(0, 3)){
  	    for(j <- List.range(0, 3)){
	      print(shape(i)(j))
	    }
	    println()
	  }
	  println()
	}
	def oRot(orientation : Int): Int = orientation match {
	  case x if x < 4 => (x+1)%4
	  case x => ((x-1)%4) + 4
	}
	def oRef(orientation : Int): Int = (orientation+4)%8
	def rot : LPosition = new LPosition((offsetY+1)-1,(3-(offsetX+1))-1,oRot(orientation))
	def ref : LPosition = new LPosition((3-(offsetX+1))-1,(offsetY+1)-1,oRef(orientation))
	def representationEquals(other : LPosition) : Boolean = {
	  for(i <- List.range(0, 3)){
  	    for(j <- List.range(0, 3)){
		  if(this.shape(i)(j)!=other.shape(i)(j)) return false
		}
	  }
	  return true
	}
  }
  //This class represents the single square pieces
  case class DPosition(val offsetX : Int, val offsetY : Int){
	assert((offsetX> -1)&&(offsetX<4),println("Error constructing DPosition: offsetX must be between 0 and 3 inclusive"))
	assert((offsetY> -1)&&(offsetY<4),println("Error constructing DPosition: offsetY must be between 0 and 3 inclusive"))
	override def hashCode : Int = offsetX+4*offsetY
	def rot : DPosition = new DPosition(offsetY,3-offsetX)
	def ref : DPosition = new DPosition(3-offsetX,offsetY)
  }
  //This companion object just contains a equality comparison of board layouts
  object LGamePosition{
  	def representationEqual(one : List[List[Int]], another : List[List[Int]]) : Boolean = {
	  for(i <- List.range(0, 4)){
	    for(j <- List.range(0, 4)){
		  if(one(i)(j)!=another(i)(j)) return false
		}
	  }
	  return true
	}
  }
  //This class represents a position that may occur during the game
  class LGamePosition(val L1 : LPosition, val L2 : LPosition, val D1 : DPosition, val D2 : DPosition, val move : Int){
    def rot : LGamePosition = new LGamePosition(L1.rot,L2.rot,D1.rot,D2.rot,move)
	def ref : LGamePosition = new LGamePosition(L1.ref,L2.ref,D1.ref,D2.ref,move)
	//We need a hashCode and equality method for uniqueness testing so we can use Set[LGamePosition] later on
	override def hashCode : Int = this.normalForm.hashCodeHelper
	def hashCodeHelper : Int = L1.hashCode+L2.hashCode*128+
	                           (D1.offsetX+D2.offsetX)*16384+(D1.offsetY+D2.offsetY)*131072+(move-1)*1048576
	def normalForm :  LGamePosition = {
	  var temp : LGamePosition = this
	  if(temp.L1.orientation>3) temp=temp.ref
//	  println(temp.L1.offsetX+" "+temp.L1.offsetY)
	  while((temp.L1.offsetX>0)||(temp.L1.offsetY>0)){
//	    println(temp.L1.offsetX+" "+temp.L1.offsetY)
	    temp=temp.rot
	  }
	  temp
	}
	//Because pieces can't overlap the type system can't determine valid positions easily. Hence we use Option.
	val boardLayout : Option[List[List[Int]]] = {
	  var valid = true
	  if((!L1.isValid)||(!L2.isValid)){
	    None
	  } else {
	    val rValue : Array[Array[Int]] = Array(Array(0,0,0,0),
											   Array(0,0,0,0),
											   Array(0,0,0,0),
											   Array(0,0,0,0))
	    for(i <- List.range(0, 3)){
  	      for(j <- List.range(0, 3)){
		    if((i+L1.offsetX> -1)&&(i+L1.offsetX<4)&&(j+L1.offsetY> -1)&&(j+L1.offsetY<4)){
		      if(rValue(i+L1.offsetX)(j+L1.offsetY)==0)
 		        rValue(i+L1.offsetX)(j+L1.offsetY)=L1.shape(i)(j)
		       else
		        if(L1.shape(i)(j)!=0) valid=false
			}
			if((i+L2.offsetX> -1)&&(i+L2.offsetX<4)&&(j+L2.offsetY> -1)&&(j+L2.offsetY<4)){
		      if(rValue(i+L2.offsetX)(j+L2.offsetY)==0)
 		        rValue(i+L2.offsetX)(j+L2.offsetY)=2*L2.shape(i)(j)
			  else
		        if(L2.shape(i)(j)!=0) valid=false
		    }
		  }
	    }
		if(rValue(D1.offsetX)(D1.offsetY)==0) rValue(D1.offsetX)(D1.offsetY)=3 else valid=false
		if(rValue(D2.offsetX)(D2.offsetY)==0) rValue(D2.offsetX)(D2.offsetY)=3 else valid=false
	    if(valid) Some(rValue.map(j => j.toList).toList) else None
	  }
	}
	//As mentioned above validity cannot be tracked just with the type system
    val isValid : Boolean = boardLayout match {
      case None => false
	  case Some(_) => true
	}
	def displayBoard() = boardLayout match {
	  case None => println("Error cannot print board layout: The board layout is not defined as the position specified wasn't valid!");
	  case Some(board) => {
  	    for(i <- List.range(0, 4)){
	      for(j <- List.range(0, 4)){
		    print(board(i)(j))
		  }
		  println()
		}
		println()
	  }
	}
    //Because positions can be invalid we would want to use an option here. But then we wouldn't be able to use sets.	
	override def equals(o: Any) : Boolean = o match {
	  case other : LGamePosition => {
	    if(this.move!=other.move) return false
	    (this.boardLayout, other.boardLayout) match {
        case (None, _) => throw new Exception("Error: Cannot compare boards for equality - At least one board is invalid.")
        case (_, None) => throw new Exception("Error: Cannot compare boards for equality - At least one board is invalid.")		
        case (Some(t),Some(o)) => 
  	      for(s <- List.range(0,8)){
		    if(LGamePosition.representationEqual(t, applySymmetry(s,o))) return true
		  }
	    }
	    return false
	  }
	  case _ => false
	}
	/*def normalForm : LGamePosition = {
	  
	}*/
  }
  lazy val allLGamePositions : Set[LGamePosition] = {
    var rawLGamePositions : Set[LGamePosition] = Set()
	for(L1X <- List.range(-1, 3); L1Y <- List.range(-1, 3);L1O <- List.range(0,8)){
	  val L1 = new LPosition(L1X,L1Y,L1O)
	  if(L1.isValid) {
	    for(L2X <- List.range(-1, 3); L2Y <- List.range(-1, 3);L2O <- List.range(0,8)){
		  val L2 = new LPosition(L2X,L2Y,L2O)
	      if(L2.isValid) {
		    for(D1X <- List.range(0, 4); D1Y <- List.range(0, 4);
                D2X <- List.range(0, 4); D2Y <- List.range(0, 4);
				move <- List.range(1,3)) {
	          val LGamePos = new LGamePosition(L1,L2,new DPosition(D1X,D1Y),new DPosition(D2X,D2Y), move)
			  if(LGamePos.isValid) rawLGamePositions+=LGamePos
			}
		  }
	    }
	  }
	}
	rawLGamePositions
  }
  def main(args: Array[String]) = {
    
  }
}