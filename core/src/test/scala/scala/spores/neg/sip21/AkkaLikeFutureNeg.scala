package scala.spores
package neg
package basic

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test

import util._

@RunWith(classOf[JUnit4])
class AkkaLikeFutureNeg {

  val messages =
    s"""
       |case object PingMessage
       |case object PongMessage
     """.stripMargin

  @Test
  def `wrong reference to outer variables in actors`() {
    expectError("invalid reference to") {
      s"""
      import scala.spores._

      trait Actor extends AnyRef {
        def sender: ActorRef = ???
        def receive: PartialFunction[Any, Unit]
      }

      trait ActorRef {
        def !(msg: Any): Unit
      }

      $messages

      class Pong extends Actor {
        def receive = {
          case PingMessage =>
            println(" pong")
            val s = spore {
              delayed {
                sender ! PongMessage
              }
            }
        }
      }
      """
    }
  }

  val shared =
    s"""
       |import scala.spores._
       |
       |trait Actor extends AnyRef {
       |  implicit val self: ActorRef = ???
       |  def sender: ActorRef = ???
       |  def receive: PartialFunction[Any, Unit]
       |}
       |
       |object Actor {
       |  val noSender: ActorRef = ???
       |}
       |
       |trait ActorRef {
       |  def !(message: Any)(implicit sender: ActorRef = Actor.noSender): Unit
       |}
     """.stripMargin

  @Test
  def `wrong reference to outer variables in actors using implicit self`() {
    expectError("invalid reference to") {
      s"""
      import scala.spores._

      $shared
      $messages

      class Pong extends Actor {
        def receive = {
          case PingMessage =>
            println(" pong")
            val s = spore {
              delayed {
                sender ! PongMessage
              }
            }
        }
      }
      """
    }
  }

  @Test
  def `wrong reference in actor using implicit self`() {
    expectError("invalid reference to") {
      s"""
      import scala.spores._

      $shared
      $messages

      class Pong extends Actor {
        def receive = {
          case PingMessage =>
            println(" pong")
            val s = spore {
              delayed {
                self ! PongMessage
              }
            }
        }
      }
      """
    }
  }

}
