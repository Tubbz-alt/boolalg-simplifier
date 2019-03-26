package com.github.natanbc.boolalg

import java.util.logging.LogManager

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{ headers, _ }
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import com.github.natanbc.boolalg.parser.Parser
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import spray.json.{ JsArray, JsObject, JsString }

import scala.io.Source
import scala.util.Success

object WebServer extends LazyLogging {
  def main(args: Array[String]) {
    configureLogging()
    
    val config = ConfigFactory.load()
    val index = Source.fromResource("index.html").mkString
    implicit val system = ActorSystem("system", config)
    implicit val materializer = ActorMaterializer()
    implicit val dispatcher = system.dispatcher
    val route =
      get {
        pathSingleSlash {
          respondWithHeaders(headers.RawHeader("Access-Control-Allow-Origin", "*")) {
            complete { HttpEntity(ContentTypes.`text/html(UTF-8)`, index) }
          }
        } ~
        path("simplify") {
          parameter('expression) { expression =>
            val context = new SimplificationContext()
            try {
              val node = new Parser(expression).parseExpression(context)
              val res = node.simplify(context)
              respondWithHeaders(headers.RawHeader("Access-Control-Allow-Origin", "*")) {
                complete(HttpEntity(ContentTypes.`application/json`, JsObject(
                  "start" -> node.toJson,
                  "end" -> res.toJson,
                  "steps" -> JsArray(context.steps.map {
                    case (old, reason, n) => JsObject(
                      "from" -> old.toJson,
                      "to" -> n.toJson,
                      "reason" -> JsString(reason)
                    )
                  }: _*)
                ).toString()))
              }
            } catch {
              case e: Exception =>
                respondWithHeaders(headers.RawHeader("Access-Control-Allow-Origin", "*")) {
                  complete(HttpResponse(
                    status = StatusCodes.BadRequest,
                    entity = HttpEntity(ContentTypes.`application/json`, JsObject(
                      "error" -> JsString(e.getMessage)
                    ).toString())
                  ))
                }
            }
          }
        }
      }
  
    Http()
      .bindAndHandle(route,
        config.getString("http.service.bind-to"),
        config.getInt("http.service.port"))
      .andThen {
        case Success(binding) => logger.info(s"Listening at ${binding.localAddress}")
      }
  }
  
  private def configureLogging(): Unit = {
    val is = getClass.getResourceAsStream("/app.logging.properties")
    try {
      LogManager.getLogManager.reset()
      LogManager.getLogManager.readConfiguration(is)
    }
    finally is.close()
  }
}
