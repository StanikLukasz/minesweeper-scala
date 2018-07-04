import scala.Array._
import scala.util.Random
import Console._
import Field

import State._


class Board (height: Int, width: Int, mines: Int){
    var board = ofDim[Field](width, height)
    
    def increaseNeighbour(x: Int, y:Int){
        if(board(x)(y).state == Empty) board(x)(y).state = Number
        board(x)(y).value += 1
    }

    def icreaseNeighbourhood(x: Int, y:Int): Unit ={
        if(x > 0 && y > 0) increaseNeighbour(x-1,y-1)
        if(y > 0) increaseNeighbour(x,y-1)
        if(y > 0 && x < height-1) increaseNeighbour(x+1,y-1)
        if(x > 0) increaseNeighbour(x-1,y)
        if(x < height-1) increaseNeighbour(x+1,y)
        if(y < width-1 && x > 0) increaseNeighbour(x-1,y+1)
        if(y > width-1) increaseNeighbour(x,y+1)
        if(y > width-1 && x < height-1) increaseNeighbour(x+1,y+1)
    }

    def prepareBoard(){
        var r = new Random
        var placeForMine = 0
        for (i <- 1 to mines){
            do{
                placeForMine = r.nextInt(height*width-1)
            }
            while(board(placeForMine/width)(placeForMine%width).state!=Empty)
            var x = placeForMine/width
            var y = placeForMine%width
            board(x)(y).state = Mine

            increaseNeighbourhood(x,y)
        }
    }

    def checkField(x: Int, y: Int){
        if(!board(x)(y).hidden){
            println("Pole juz odsloniete")

        }
        else board(x)(y).state match {
            case Empty => handleEmpty(x,y)
            case Mine => endGame()
            case Number => showNumber(x,y)
        }
    }

    def printBoard(){
        println("   ")
        for(i <- 0 to width-1) println("("+i+") ")
        
        for(i <- 0 to height-1){
            println("("+i+") ")
            for(j <- 0 to width-1){
                if(board(j)(i).hidden==true) println("[ ] ")
                else board(j)(i).state match{
                    case Empty => println("   ")
                    case Mine => println("O ")
                    case Number => println(board(j)(i).value+" ")
                }
            }
        }
    }

    def checkWin():Boolean = {
        var won: Boolean = true
        for(i <- 0 to width-1){
            for(j <- 0 to height-1){
                if(board(i)(j).hidden==true && board(i)(j).state != Mine) won = false
            }
        }
        return won
    }


}

