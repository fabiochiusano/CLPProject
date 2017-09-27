package toolc
package utils

import java.io.File
import scala.io.Source

trait Positioned {
  /** Number of bits used to encode the line number */
  final private[this] val LINE_BITS   = 20
  /** Number of bits used to encode the column number */
  final private[this] val COLUMN_BITS = 31 - LINE_BITS // no negatives => 31
  /** Mask to decode the line number */
  final private[this] val LINE_MASK   = (1 << LINE_BITS) - 1
  /** Mask to decode the column number */
  final private[this] val COLUMN_MASK = (1 << COLUMN_BITS) - 1 

  private[this] def lineOf(pos: Int): Int = (pos >> COLUMN_BITS) & LINE_MASK
  private[this] def columnOf(pos: Int): Int = pos & COLUMN_MASK

  private[Positioned] var _file: Option[File] = None
  private[Positioned] var _line: Int = 0
  private[Positioned] var _col: Int = 0
  
  def setPos(file: File, pos: Int): this.type = {
    // Remember that position is not relative to the file in input, but to the file in input concatenated with the library
    import scala.io.Position
    
    val lib = new File("./gui_lib/GUI.tool")
    val t1 = Source.fromFile(file).mkString
    val linesFile1 = t1.lines.size
    
    if (lineOf(pos) <= linesFile1) { // If the position is in the file in input
      _line = lineOf(pos)
      _col  = columnOf(pos)
      _file = Some(file)
    }
    else { // If the position is in the library GUI
      _line = lineOf(pos) - linesFile1
      _col  = columnOf(pos)
      _file = Some(lib)
    }

    this
  }

  def hasPosition = _file.isDefined

  def setPos(other: Positioned): this.type = {
    _line = other._line
    _col  = other._col
    _file = other._file

    this
  }


  def file = _file.get
  def line = _line
  def col  = _col

  def position: String = {
    if (hasPosition) {
      file.getPath+":"+line+":"+col
    } else {
      "?:?"
    }
  }
}

case object NoPosition extends Positioned
