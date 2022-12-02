package adventofcode

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class Day1Spec extends AnyWordSpec with Matchers with AdventOfCodeHelper {
  val inputFilename = "day1/input.txt"

  private def textToInts(lines: String) : Seq[Option[Int]] = {
    lines
      .split(System.lineSeparator())
      .map(line => line.trim)
      .map({
          case "" => None
          case line => Some(line.toInt)
        })
      .toList
  }

  val dataInts : Seq[Option[Int]] = textToInts(data)

  val exampleText : String = """
1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
"""
  val exampleInts : Seq[Option[Int]] = textToInts(exampleText)

  def assertAndPrint[T](message: String, answer: T, expected: T) = {
    // println(s"${message.padTo(30, ' ')} - $answer")


    
    answer shouldBe expected
  }

  def solveA(textInput : String ) : Int = {
    
    val ints = textToInts(textInput)

    def splitIntoChunks(list: Seq[Option[Int]]) : Seq[Int] = {
      
      val elves = Array.newBuilder[Int]
      
      var total = 0
      for (x <- list){
        x match {
          case None => {
            elves += total
            total = 0
          }
          case Some(v) => total+=v
        }
      }

      elves.result()
    }
     
    val elves : Seq[Int] = splitIntoChunks(ints)
    elves.max
  }
  
  "Day 1" should {
    "Part 1 Example" in {
      assertAndPrint("Part 1 example", solveA(exampleText), 24000)
    }

    "Part 1" in {
       assertAndPrint("Part 1", solveA(data), 69795)
     }
  }
}
