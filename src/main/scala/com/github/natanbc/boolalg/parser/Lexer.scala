package com.github.natanbc.boolalg.parser

import java.io.PushbackReader

import scala.collection.mutable

class Lexer(r: PushbackReader) {
  private val lineMap = new mutable.HashMap[Int, mutable.StringBuilder]()
  private var line = 1
  private var column = 0
  private var nextToken: Token = _
  private val tokens = Stream.continually(parse()).takeWhile(_.kind != TokenKind.Eof).toIndexedSeq
  private var idx = 0
  
  def next(): Token = {
    if(nextToken != null) {
      val v = nextToken
      nextToken = null
      return v
    }
    if(tokens.size <= idx) {
      return Token(TokenKind.Eof, pos, "<EOF>")
    }
    val v = tokens(idx)
    idx += 1
    v
  }
  
  def push(t: Token): Unit = {
    if(nextToken != null) {
      throw new IllegalStateException("Next token already set!")
    }
    nextToken = t
  }
  
  def before(t: Token, n: Int = 1): Option[Token] = {
    val idx = tokens.indexOf(t)
    if(idx < 0) {
      throw new IllegalArgumentException("The provided token doesn't belong to this lexer")
    }
    if(n < 1) {
      throw new IllegalArgumentException("N < 1")
    }
    if(idx < n) {
      None
    } else {
      Some(tokens(idx - n))
    }
  }
  
  private def parse(): Token = {
    read() match {
      case -1 => Token(TokenKind.Eof, pos, "<EOF>")
      case '0' => Token(TokenKind.Value, pos, "0")
      case '1' => Token(TokenKind.Value, pos, "1")
      case '~' | '!' => Token(TokenKind.Neg, pos, "!")
      case '+' => Token(TokenKind.Or, pos, "+")
      case '*' => Token(TokenKind.And, pos, "*")
      case '(' => Token(TokenKind.LeftParen, pos, "(")
      case ')' => Token(TokenKind.RightParen, pos, ")")
      case c if c.toChar.isLetter => Token(TokenKind.Input, pos, readName(c.toChar))
      case c =>
        val (line, column) = pos
        val s = prettyContext(pos)
  
        throw new IllegalArgumentException(s"Unexpected character ${c.toChar} at line $line, column $column\n\n$s")
    }
  }
  
  def context(pos: (Int, Int), around: Int = 5, readAfter: Boolean = true): (String, Int, Int) = {
    val (line, column) = pos
    if(line == this.line && readAfter) {
      //skip to next line or eof (fill line buffer for context in the error message)
      val l = line
      while(this.line == l && read() != -1) {}
    }
    val buffer = lineBuffer(line)
    val before = math.min(column - 1, around)
    val after = math.min(buffer.length - column, around)
    (buffer.substring(math.max(column - around - 1, 0), math.min(column + around, buffer.length)), before, after)
  }
  
  def prettyContext(pos: (Int, Int), around: Int = 5, readAfter: Boolean = true): String = {
    val (ctx, before, _) = context(pos)
    ctx + "\n" + (" " * before) + "^"
  }
  
  def pos: (Int, Int) = (line, column)
  
  private def readName(c: Char): String = {
    val sb = new StringBuilder().append(c)
    var ch = read(skipWhitespace = false)
    while(ch != -1 && ch.toChar.isLetterOrDigit) {
      sb.append(ch.toChar)
      ch = read(skipWhitespace = false)
    }
    if(ch != -1) {
      unread(ch)
    }
    sb.toString()
  }
  
  private def unread(ch: Int): Unit = {
    column -= 1
    lineBuffer().length -= 1
    r.unread(ch)
  }
  
  private def read(skipWhitespace: Boolean = true): Int = {
    while(true) {
      val c = r.read()
      if(c == -1) {
        return -1
      }
      c.toChar match {
        case '\n' => line += 1; column = 0
        case ch if ch.isWhitespace && skipWhitespace => column += 1; lineBuffer() += ch
        case ch => column += 1; lineBuffer() += ch; return ch
      }
    }
    throw new AssertionError()
  }
  
  private def lineBuffer(line: Int = this.line): mutable.StringBuilder = {
    lineMap.getOrElseUpdate(line, new StringBuilder())
  }
}
