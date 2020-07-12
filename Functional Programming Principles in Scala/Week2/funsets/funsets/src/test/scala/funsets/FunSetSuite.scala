package funsets

import org.junit._

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite {

  import FunSets._

  @Test def `contains is implemented`: Unit = {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remvoe the
   * @Ignore annotation.
   */
  @Test def `singleton set one contains one`: Unit = {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  @Test def `union contains all elements of each set`: Unit = {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  @Test def `intersection contains all common elements if two sets`: Unit = {
    new TestSets {
      val s = intersect(union(s1, s2), union(s3, s2))
      assert(!contains(s, 1), "Intersection absent")
      assert(contains(s, 2), "Intersection present")
      assert(!contains(s, 3), "Intersection absent")
    }
  }

  @Test def `difference contains all elements of (s1 union s2) that are not in (s3 union s2)`: Unit = {
    new TestSets {
      val s = diff(union(s1, s2), union(s3, s2))
      assert(contains(s, 1), "Difference present")
      assert(!contains(s, 2), "Intersection absent")
      assert(!contains(s, 3), "Intersection absent")
    }
  }

  @Test def `filter returns a set of all elements that satisfy the given function`: Unit = {
    new TestSets {
      val s = filter(union(s1, union(s3, s2)), (x: Int) => x%2 == 0)
      assert(!contains(s, 1), "Filter absent")
      assert(contains(s, 2), "Filter present")
      assert(!contains(s, 3), "Filter absent")
    }
  }

  @Test def `forall returns a true if all elements in set satisfy the given function else false`: Unit = {
    new TestSets {
      assert(!forall(union(s1, union(s3, s2)), (x: Int) => x%2 == 0), "forall false")
      assert(forall(union(s1, union(s3, s2)), (x: Int) => x > 0), "forall true")
    }
  }

  @Test def `exists returns a true if any element in set satisfies the given function else false`: Unit = {
    new TestSets {
      assert(!exists(union(s1, union(s3, s2)), (x: Int) => x < 0), "exists false")
      assert(exists(union(s1, union(s3, s2)), (x: Int) => x%2 == 0), "exists true")
    }
  }

  @Test def `map returns a set transformed by applying the given function to each element of the set`: Unit = {
    new TestSets {
      val s = map(union(s1, union(s3, s2)), (x: Int) => x*x)
      assert(contains(s, 1), "map")
      assert(contains(s, 4), "map")
      assert(contains(s, 9), "map")
      assert(!contains(s, 2), "map")
      assert(!contains(s, 3), "map")
    }
  }



  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
