package layoutz

import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable.ListBuffer

/** Test helpers living in the layoutz package so they can inspect the package-private Cmd and Sub implementations. */
object TestKit:

  /** Flatten a command into its leaf commands */
  def flatten[Msg](cmd: Cmd[Msg]): List[Cmd[Msg]] = cmd match
    case CmdBatch(cmds) => cmds.flatMap(flatten)
    case other          => List(other)

  def isNone(cmd: Cmd[?]): Boolean = flatten(cmd).forall(_ == CmdNone)

  def hasExit(cmd: Cmd[?]): Boolean = flatten(cmd).contains(CmdExit)

  def hasHideCursor(cmd: Cmd[?]): Boolean = flatten(cmd).contains(CmdHideCursor)

  def titles(cmd: Cmd[?]): List[String] = flatten(cmd).collect { case CmdSetTitle(t) => t }

  def taskCount(cmd: Cmd[?]): Int = flatten(cmd).count(_.isInstanceOf[CmdTask[?, ?]])

  def fireCount(cmd: Cmd[?]): Int = flatten(cmd).count(_.isInstanceOf[CmdFire])

  /** Synchronously run every task and fire effect of the command, returning the messages produced by the tasks */
  def run[Msg](cmd: Cmd[Msg]): List[Msg] =
    flatten(cmd).flatMap {
      case CmdTask(task, toMsg) => List(toMsg(task()))
      case CmdFire(effect)      => effect(); Nil
      case _                    => Nil
    }

  private def flattenSubs[Msg](sub: Sub[Msg]): List[Sub[Msg]] = sub match
    case SubNone        => Nil
    case SubBatch(subs) => subs.flatMap(flattenSubs)
    case other          => List(other)

  def keyHandler[Msg](sub: Sub[Msg]): Key => Option[Msg] =
    flattenSubs(sub).collectFirst { case OnKeyPress(h) => h }.getOrElse(_ => None)

  def timers[Msg](sub: Sub[Msg]): List[(Long, Msg)] =
    flattenSubs(sub).collect { case OnTimeEveryMs(ms, gen) => ms -> gen() }

  /** A fake terminal fed with scripted keys.
    *
    * Each app run (from enterRawMode to close) is a new session reading its own script. A read pending when a session closes returns EOF so the input thread of
    * a finished app does not swallow keys of the next one.
    */
  final class ScriptedTerminal(scripts: List[List[ScriptedTerminal.Step]]) extends Terminal:
    private val session = new AtomicInteger(-1)
    private val queues  = scripts.map(steps => new LinkedBlockingQueue[ScriptedTerminal.Step](java.util.Arrays.asList(steps*)))
    val output          = ListBuffer.empty[String]

    override def enterRawMode(): Unit          = session.incrementAndGet(): Unit
    override def exitRawMode(): Unit           = ()
    override def clearScreen(): Unit           = ()
    override def clearScrollback(): Unit       = ()
    override def hideCursor(): Unit            = ()
    override def showCursor(): Unit            = ()
    override def write(text: String): Unit     = output.synchronized(output += text): Unit
    override def writeLine(text: String): Unit = write(text + "\n")
    override def flush(): Unit                 = ()
    override def terminalWidth(): Int          = 200
    override def close(): Unit                 = session.incrementAndGet(): Unit

    override def readInput(): Int =
      val current = session.get
      val queue   = queues.lift(current / 2) // enterRawMode and close both open a new session
      var result  = Option.empty[Int]
      while result.isEmpty do
        if session.get != current then result = Some(-1)
        else
          queue.flatMap(q => Option(q.poll(10, TimeUnit.MILLISECONDS))) match
            case Some(ScriptedTerminal.Step(delayMs, code)) =>
              if delayMs > 0 then Thread.sleep(delayMs)
              result = Some(code)
            case None => if queue.isEmpty then Thread.sleep(10)
      result.get

    override def readInputNonBlocking(): Option[Int] = None

  object ScriptedTerminal:
    final case class Step(delayMs: Long, code: Int)
    val Enter: Int                              = '\r'
    val Escape: Int                             = 27
    def key(code: Int, delayMs: Long = 0): Step = Step(delayMs, code)
