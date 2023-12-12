package adder

import chisel3._
import org.chipsalliance.cde.config._

import scala.annotation.tailrec

case object WidthKey extends Field[Int]

class DefaultConfig(width: Int = 32)
  extends Config((site, here, up) => {
    case WidthKey => width
  })

class WithDoubleWidth
  extends Config((site, here, up) => {
    case WidthKey => {
      println(s"double: up(WidthKey) is ${up(WidthKey)}")
      up(WidthKey) * 2
    }
  })

class DoubleConfig(width: Int = 32)
  extends Config(
    new DefaultConfig(width = width)
        .alter(new WithDoubleWidth)
  )

object Adder extends App {
  def configInstanceFromString(configName: String, prefix: String = this.getClass.getPackageName, width: Int = 32): Parameters = {
    val fullPath = if (configName.contains('.')) configName else prefix + '.' + configName
    val constructor = Class.forName(fullPath).getConstructor(Integer.TYPE)
    constructor.newInstance(width.asInstanceOf[Object]).asInstanceOf[Parameters]
  }

  println("Start")

  @tailrec
  def nextOption(config: Parameters, list: List[String]): Parameters = {
    list match {
      case Nil => config
      case ("-c" | "--config") :: configName :: tail =>
        println(s"using config $configName")
        nextOption(configInstanceFromString(configName, width = config(WidthKey)), tail)
      case ("-w" | "--width") :: width :: tail =>
        println(s"setting width to $width")
        nextOption(config.alter((site, here, up) => {
          case WidthKey => width.toInt
        }), tail)
      case unknownOption :: tail =>
        println(s"unknown option: $unknownOption")
        nextOption(config, tail)
    }
  }

  private val config = nextOption(new DefaultConfig(), args.toList)
  println(s"config: $config")
  println(s"config.width is ${config(WidthKey)}")
}