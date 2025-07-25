/**
Getting Started
===============

Outline:
<div id="tableofcontents"></div>

Staging and LMS
---------------

What is staging? The idea behind staging is to delay computation of
certain expressions, generating code to compute them later. The
benefit is *abstraction without regret*: using high-level programming
abstractions once, to structure generated code, instead of all the
time during execution.

Lightweight Modular Staging (LMS) is a staging technique driven by
types. In addition to the *staging* aspect, the technique is *lightweight*,
because it is purely library based. It is also *modular*, because
features can be mixed and matched and the framework is easy to
extend.


My First Multi-Stage Program
------------------------------

This tutorial is a [literate Scala file](https://github.com/scala-lms/tutorials/tree/master/src/test/scala/lms/tutorial/start.scala). We invite you to clone the
[GitHub repo](https://github.com/scala-lms/tutorials/) and play with the code as you follow along.

*/




package scala.lms.tutorial

import scala.lms._
import scala.lms.common._

@virtualize
class GettingStartedTest extends TutorialFunSuite {
  val under = "dslapi"


/**
### Rep[T] vs T

In LMS, `Rep[T]` represents a delayed computation of type `T`. Thus,
during staging, an expression of type `Rep[T]` becomes part of
the generated code, while an expression of bare type `T` becomes a
constant in the generated code. For core Scala features, adding
`Rep` types should be enough to build a program generator, as we
will see later.
*/

  test("1") {
    object Snippet extends DslDriver[Int, Int] {
      def snippet(x: Rep[Int]) = {

        def compute(b: Boolean): Rep[Int] = {
          // the if is executed in the first stage
          if (b) 1 else x
        }
        compute(true)+compute(1==1)
      }
    }

    check("1", Snippet.code)
    //assert(snippet.eval(0) === 2)
  }
/**
      .. includecode:: ../../../../out/dslapi1.check.scala

Contrast the snippet above, where `b` is a `Boolean`, with the snippet
below, where `b` is a `Rep[Boolean]`. The expression `if (b) 1 else x`
is executed at staging time when `b` is a `Boolean`, while it is delayed
causing code to be generated for the `if` expression when `b` is a
`Rep[Boolean]` -- indeed, the actual value of `b` is not known at staging
time, but only when the generated code is executed.
*/

  test("2") {
    object Snippet extends DslDriver[Int,Int]{
      def snippet(x: Rep[Int]) = {
        def compute(b: Rep[Boolean]): Rep[Int] = {
          // the if is deferred to the second stage
          if (b) 1 else x
        }
        compute(x==1)
      }
    }
    check("2", Snippet.code)
    //assert(snippet.eval(2) === 2)
  }
/**
      .. includecode:: ../../../../out/dslapi2.check.scala


### Rep[A => B] vs Rep[A]=>Rep[B]

In the previous snippets, we already notice some *abstraction without
regret*: the ``compute`` function exists only at staging time, and is
not part of the generated code -- more generally, we can freely use
abstractions to structure and compose the staged program, but these
abstractions are not part of the generated code when their type is a
bare ``T`` as opposed of a ``Rep[T]``. In the second snippet,
``compute`` has the type ``Rep[Boolean] => Rep[Int]``, not
``Rep[Boolean => Int]`` -- its type already tells us that the function
is known at staging time.

Similarly, below, the recursive power function and the helper square
function only exists during staging time.
*/

  test("power") {
    object Snippet extends DslDriver[Int,Int] {
      def square(x: Rep[Int]): Rep[Int] = x*x

      def power(b: Rep[Int], n: Int): Rep[Int] =
        if (n == 0) unit(1)
        else if (n % 2 == 0) square(power(b, n/2))
        else b * power(b, n-1)

      def snippet(b: Rep[Int]) =
        power(b, 7)

    }
    check("power", Snippet.code)
    //assert(snippet.eval(2) === 128)
  }

/**

Because of common subexpression elimination, we get reuse of the square
argument for free.

      .. includecode:: ../../../../out/dslapipower.check.scala

We could also create a generated square function, of type
`Rep[Int=>Int]` instead of `Rep[Int]=>Rep[Int]`.
*/

  test("power with fun square") {
    val snippet = new DslDriver[Int,Int] {
      def square: Rep[Int=>Int] = fun {x => x*x}

      def power(b: Rep[Int], n: Int): Rep[Int] =
        if (n == 0) 1
        else if (n % 2 == 0) square(power(b, n/2))
        else b * power(b, n-1)

      def snippet(b: Rep[Int]) =
        power(b, 7)

    }
    check("powerfunsquare", snippet.code)
    //assert(snippet.eval(2) === 128)
  }

/**

The code we get is in fact slightly less efficient, because of these
extra calls to the generated square function.

      .. includecode:: ../../../../out/dslapipowerfunsquare.check.scala

### Rep[Range] vs Range

Loops can be unrolled in the first stage, or be generated as loops in
the second stage, driven by the type of their condition.
*/

  test("range1") {
    val snippet = new DslDriver[Int,Unit] {
      def snippet(x: Rep[Int]) = comment("for", verbose = false) {

        for (i <- (0 until 3): Range) {
          println(i)
        }

      }
    }
    check("range1", snippet.code)
  }
/**
      .. includecode:: ../../../../out/dslapirange1.check.scala for
*/

  test("range2") {
    val snippet = new DslDriver[Int,Unit] {
      def snippet(x: Rep[Int]) = comment("for", verbose = false) {

        for (i <- (0 until x): Rep[Range]) {
          println(i)
        }

      }
    }
    check("range2", snippet.code)
  }
}
/**
      .. includecode:: ../../../../out/dslapirange2.check.scala for


What's next?
------------

Go back to the [tutorial index](index.html) or continue with the [Shonan Challenge](shonan.html).

*/
