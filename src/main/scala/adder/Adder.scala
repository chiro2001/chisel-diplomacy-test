package adder

import chisel3._
import chisel3.stage._
import circt.stage.ChiselStage
import chisel3.experimental.SourceInfo
import chisel3.util.random.FibonacciLFSR
import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._

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

case class UpwardParam(width: Int)

case class DownwardParam(width: Int)

case class EdgeParam(width: Int)

object AdderNodeImpl extends SimpleNodeImp[DownwardParam, UpwardParam, EdgeParam, UInt] {

  /** Creates the edge parameters by combining the downward-flowing and upward-flowing parameters for edges that connect to this node.
   *
   * It is left up to a user defining a particular protocol implementation to decide how the parameters flowing through the graph in
   * both directions are combined into a single representation on an Edge.
   *
   * @param pd         The downward-flowing parameters into the node along the edge.
   * @param pu         The upward-flowing parameters going out of the node along the edge.
   * @param p          [[Parameters]]s which can be used during negotiation.
   * @param sourceInfo [[SourceInfo]] of this edge.
   * @return Negotiated edge parameters.
   */
  override def edge(pd: DownwardParam, pu: UpwardParam, p: Parameters, sourceInfo: SourceInfo): EdgeParam = {
    if (pd.width < pu.width) EdgeParam(pd.width) else EdgeParam(pu.width)
  }

  /** Generate the Bundle from the negotiated Edge parameters.
   *
   * @param e the negotiated Edge parameters
   * @return the corresponding Bundle of this node
   */
  override def bundle(e: EdgeParam): UInt = UInt(e.width.W)

  /** Define how the edge should be rendered (e.g. in GraphML).
   *
   * @param e Edge to render.
   * @return [[RenderedEdge]] description of how the edge should be generated.
   */
  override def render(e: EdgeParam): RenderedEdge = RenderedEdge("blue", s"width = ${e.width}")
}

class AdderDriverNode(widths: Seq[DownwardParam])(implicit valName: ValName)
  extends SourceNode(AdderNodeImpl)(widths)

class AdderMonitorNode(width: UpwardParam)(implicit valName: ValName)
  extends SinkNode(AdderNodeImpl)(Seq(width))

class AdderNode(dFn: Seq[DownwardParam] => DownwardParam,
                uFn: Seq[UpwardParam] => UpwardParam)(implicit valName: ValName)
  extends NexusNode(AdderNodeImpl)(dFn, uFn)

class Adder(implicit p: Parameters) extends LazyModule {
  val node = new AdderNode(
    {
      dps: Seq[DownwardParam] =>
        dps.head
    },
    {
      ups: Seq[UpwardParam] =>
        ups.head
    }
  )

  /** [[chisel3]] hardware implementation of this [[LazyModule]].
   *
   * Subclasses should define this function as `lazy val`s for lazy evaluation.
   * Generally, the evaluation of this marks the beginning of phase 2.
   */
  lazy val module = new LazyModuleImp(wrapper = this) {
    node.out.head._1 := node.in.map(_._1).reduce(_ + _)
  }
}

class AdderDriver(width: Int)(implicit p: Parameters) extends LazyModule {
  val node = new AdderDriverNode(Seq.fill(2)(DownwardParam(width)))

  /** [[chisel3]] hardware implementation of this [[LazyModule]].
   *
   * Subclasses should define this function as `lazy val`s for lazy evaluation.
   * Generally, the evaluation of this marks the beginning of phase 2.
   */
  lazy val module = new AdderDriverImpl(this)
}

class AdderDriverImpl(outer: AdderDriver) extends LazyModuleImp(wrapper = outer) {
  val finalWidth = outer.node.edges.out.head.width
  val value = FibonacciLFSR.maxPeriod(finalWidth)
  outer.node.out.foreach { case (addend, _) => addend := value }
}

class AdderMonitor(width: Int, numOps: Int)(implicit p: Parameters) extends LazyModule {
  val nodeFromDrivers = Seq.fill(numOps) {
    new AdderMonitorNode(UpwardParam(width))
  }
  val nodeFromAdder = new AdderMonitorNode(UpwardParam(width))

  /** [[chisel3]] hardware implementation of this [[LazyModule]].
   *
   * Subclasses should define this function as `lazy val`s for lazy evaluation.
   * Generally, the evaluation of this marks the beginning of phase 2.
   */
  lazy val module = new AdderMonitorImpl(this)
}

class AdderMonitorImpl(outer: AdderMonitor) extends LazyModuleImp(wrapper = outer) {
  val io = IO(new Bundle {
    val ok = Output(Bool())
  })
  io.ok := outer.nodeFromAdder.in.head._1 === outer.nodeFromDrivers.map(_.in.head._1).reduce(_ + _)
}

class AdderTestHarness()(implicit p: Parameters) extends LazyModule {
  val numOps = 3
  val drivers = Seq.fill(numOps)(LazyModule(new AdderDriver(8)))
  val monitor = LazyModule(new AdderMonitor(4, numOps))
  val adder = LazyModule(new Adder)

  drivers.foreach(adder.node := _.node)
  drivers.zip(monitor.nodeFromDrivers).foreach { case (driver, node) => node := driver.node }
  monitor.nodeFromAdder := adder.node

  /** [[chisel3]] hardware implementation of this [[LazyModule]].
   *
   * Subclasses should define this function as `lazy val`s for lazy evaluation.
   * Generally, the evaluation of this marks the beginning of phase 2.
   */
  lazy val module = new AdderTestHarnessImpl(this)
}

class AdderTestHarnessImpl(outer: AdderTestHarness) extends LazyModuleImp(wrapper = outer) {
  val cnt = RegInit(0.U(32.W))
  cnt := cnt + 1.U
  val io = IO(new Bundle {
    val ok = Output(Bool())
    val done = Output(Bool())
  })
  io.ok := outer.monitor.module.io.ok
  when(!io.ok) {
    printf("not ok...\n")
  }
  io.done := cnt >= 0xff.U
}

class AdderDiplomacy()(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val ok = Output(Bool())
    val done = Output(Bool())
  })
  val lazyDut = LazyModule(new AdderTestHarness())
  val dut = Module(lazyDut.module)
  io <> dut.io
}

object AdderApp extends App {
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

  (new ChiselStage).execute(Array("--target", "systemverilog", "--target-dir", "out"),
    Seq(ChiselGeneratorAnnotation(() => new AdderDiplomacy()(Parameters.empty))))
}