package chapter02

import org.scalatest.{Matchers, WordSpecLike}

import scala.annotation.tailrec

class Chapter02Spec extends WordSpecLike with Matchers {

  "Exercise 2.1: Fibonacci sequence" should {

    "yield 0 for the zeroth number" in new Fibonacci {
      fib(0) should be(0)
    }

    "yield 1 for the first number" in new Fibonacci {
      fib(1) should be(1)
    }

    "yield 1 for the second number" in new Fibonacci {
      fib(2) should be(1)
    }

    "yield 2 for the third number" in new Fibonacci {
      fib(3) should be(2)
    }

    "yield 3 for the fourth number" in new Fibonacci {
      fib(2) should be(1)
    }

    "yield 5 for the fifth number" in new Fibonacci {
      fib(2) should be(1)
    }
  }

  trait Fibonacci {
    def fib(n: Int): Int = {
      @tailrec
      def fib(i: Int, n1: Int, n2: Int): Int =
        if (i == n) n1 + n2
        else fib(i + 1, n2 + n1, n1)

      if (n == 0) 0
      else if (n == 1) 1
      else fib(2, 1, 0)
    }
  }
}
