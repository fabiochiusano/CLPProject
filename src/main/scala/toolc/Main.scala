package toolc

import utils._
import java.io.File
import scala.sys.process._

import lexer._
import ast._
import eval._
import analyzer._
import code._

object Main {

  def processOptions(args: Array[String]): Context = {
    val reporter = new Reporter()
    var files: List[File] = Nil
    var outDir: Option[File] = None

    def rec(args: List[String]): Unit = args match {
      case "-d" :: dir :: xs =>
        outDir = Some(new File(dir))
        rec(xs)

      case f :: xs =>
        files  ::= new File(f)
        rec(xs)

      case _ =>
    }

    rec(args.toList)

    if (files.size != 1) {
      reporter.fatal("Exactly one file expected, "+files.size+" file(s) given.")
    }

    Context(reporter = reporter, file = files.head, outDir = outDir)
  }


  def main(args: Array[String]) {
    val ctx = processOptions(args)

    val pipeline = Lexer andThen
                   Parser andThen
                   NameAnalysis andThen
                   TypeChecking andThen
                   CodeGeneration

    pipeline.run(ctx)(ctx.file)

    ctx.reporter.terminateIfErrors
    
    // Custom extension: compile ToolGUI.java and put class files in the output directory
    val outDir = ctx.outDir.map(_.getPath + "/").getOrElse("./")
    Process(Seq("javac", "-d", outDir, "./gui_lib/ToolGUI.java")).run()
  }
}
