package me.lightspeed7.args

import org.scalatest.{FunSuite, Matchers}


class CommandLineArgsTest extends FunSuite with Matchers {

  import CommandLineArgsTest._

  test("test patterns") {
    val result = toMap(List("--foo", "bar", "--", "/home/user/some/directory/filename"))
    println(result)
    result.size should be(2)
    result('foo) should be("bar")
    result('file) should be("/home/user/some/directory/filename")
  }
}

object CommandLineArgsTest {
  type OptionMap = Map[Symbol, Any]

  @annotation.tailrec
  def toMap(args: List[String], result: OptionMap = Map()): OptionMap = {

    def isSwitch(s: String): Boolean = (s.size == 2 && s(0) == '-' && s(1) == '-')

    args match {
      case Nil => result
      case "--foo" :: value :: tail => toMap(tail, result ++ Map('foo -> value.trim))
        //
      case switch :: filename :: Nil if isSwitch(switch) => result ++ Map('file -> filename.trim)
      case _ => println("Unknown argument"); sys.exit(1);
    }
  }
}