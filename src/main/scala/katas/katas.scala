package katas

import zio.*
import zio.test.*

def play(number: Int): UIO[String] =
  if (number % 3  == 0 && number % 5  == 0) ZIO.succeed("FizzBuzz")
  else if (number % 3  == 0 )  ZIO.succeed("Fizz")
  else if (number % 5  == 0)  ZIO.succeed("Buzz")
  else ZIO.succeed(number.toString)

def play2(number : Int) :UIO[String] =
  if  (number.toString.contains('5') && number.toString.contains('3')) play(number).map(playResult =>
    if (playResult.contains("Fizz")||playResult.contains("Buzz"))"Fizz"+playResult+"Buzz"
    else "FizzBuzz"
  )
  else if (number.toString.contains('3'))  play(number).map(playResult =>
    if (playResult.contains("Fizz")||playResult.contains("Buzz"))"Fizz"+playResult
    else "Fizz"
  )
  else if (number.toString.contains('5')) play(number).map(playResult =>
    if (playResult.contains("Fizz")||playResult.contains("Buzz")) playResult+"Buzz"
    else "Buzz"
  )
  else play(number)

object FizzBuzzTest extends ZIOSpecDefault {
      def spec = suite("kata fizzBuzz test")(
        test("1 = 1") {
      assertZIO(play(1))(Assertion.equalTo("1"))
    },
    test ("2 = 2") {
      assertZIO(play(2))(Assertion.equalTo("2"))
    },
    test ("3 = Fizz") {
      assertZIO(play(3))(Assertion.equalTo("Fizz"))
    },
    test ("4 = 4") {
      assertZIO(play(4))(Assertion.equalTo("4"))
    },
    test("5 = Buzz"){
      assertZIO(play(5))(Assertion.equalTo("Buzz"))
    },
    test("6 = Fizz"){
      assertZIO(play(6))(Assertion.equalTo("Fizz"))
    },
    test("7 = 7"){
      assertZIO(play(7))(Assertion.equalTo("7"))
    },
    test("8 = 8"){
      assertZIO(play(8))(Assertion.equalTo("8"))
    },
    test("9 = Fizz"){
      assertZIO(play(9))(Assertion.equalTo("Fizz"))
    },
    test("10 = Buzz"){
      assertZIO(play(10))(Assertion.equalTo("Buzz"))
    },
    test("15 = FizzBuzz"){
      assertZIO(play(15))(Assertion.equalTo("FizzBuzz"))
    },
  )
}

object FizzBuzz2Test extends ZIOSpecDefault {
  def spec = suite("kata fizzBuzz2 rajoute fizz si contient 3 rajoute buzz si contient 5 ")(
    test("1 = 1") {
      assertZIO(play2(1))(Assertion.equalTo("1"))
    },
    test("3 = FizzFizz"){
      assertZIO(play2(3))(Assertion.equalTo("FizzFizz"))
    },
    test("15 = FizzBuzzBuzz") {
      assertZIO(play2(15))(Assertion.equalTo("FizzBuzzBuzz"))
    },
    test("53 = FizzBuzz"){
      assertZIO(play2(53))(Assertion.equalTo("FizzBuzz"))
    },
    test("35 = FizzBuzzBuzz"){
      assertZIO(play2(35))(Assertion.equalTo("FizzBuzzBuzz"))
    }
  )
}
