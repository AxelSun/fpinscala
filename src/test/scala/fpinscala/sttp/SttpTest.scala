package fpinscala.sttp

import org.scalatest._
import com.softwaremill.sttp._
import com.softwaremill.sttp.testing.SttpBackendStub

class SttpTest extends FlatSpec with Matchers {


  implicit val testingBackend = SttpBackendStub.synchronous
    .whenRequestMatches(_.uri.path.startsWith(List("access")))
    .thenRespond(
      """
        |{"username":"ax",
        | "password":"attack"
        |}
      """.stripMargin)

  val response = sttp.get(uri"http://example.com/access").response(asString).send()

  "sending a request to access endpoint" should "return usernamne and password" in {

  }

}
