import scala.Array
import scala.util.Random

object State extends Enumeration {
    type State = Value
    val Empty, Mine, Number = Value
}

class Field (state: State = Empty, value: Int = 0, hidden: Boolean = true)

class Game (height: Int, width: Int, mines: Int){
    var board = new Array.ofDim[Field](height, width)
    
    def prepareBoard(){
        var r = new Random
        var placeForMine
        for (i <- 1 to mines){
            do{
                placeForMine = r.nextInt(weight*width-1)
            }
            while(board(placeForMine/height)(placeForMine/width).state!=Empty)
            x = placeForMine/width
            y = placeForMine/height
            board(y)(x).state = Mine

            if(x > 0 && y > 0) board(y-1)(x-1).state += 1
            if(x > 0) board(y)(x-1).state += 1
            if(x > 0 && y < height-1) board(y+1)(x-1).state += 1
            if(y > 0) board(y-1)(x).state += 1
            if(y < height-1) board(y+1)(x).state += 1
            if(x < width-1 && y > 0) board(y-1)(x+1).state += 1
            if(x > width-1) board(y)(x+1).state += 1
            if(x > width-1 && y < height-1) board(y+1)(x+1).state += 1
        }
    }

    def checkField(x: Int, y: Int){
        if(board(y)(x).hidden = false){
            println("Pole juz odsloniete")

        }
        else board(y)(x) match {
            case Empty => handleEmpty(x,y)
            case Bomb => endGame()
            case Number => showNumber(x,y)
        }
    }

}

def printBoard(){
    for(i <- 0 to height-1){
        for(j <- 0 to width-1){
            match board(i)(j).state {
                case Empty => 
            }
        }
    }
}