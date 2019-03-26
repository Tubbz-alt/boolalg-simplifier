package com.github.natanbc.boolalg.parser

import java.io.PushbackReader

class Lexer(r: PushbackReader) {
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
      case c => Token(TokenKind.Input, pos, readName(c.toChar))
    }
  }
  
  private def pos: (Int, Int) = (line, column)
  
  private def readName(c: Char): String = {
    val sb = new StringBuilder().append(c)
    var ch = read()
    while(ch != -1 && ch.toChar.isLetterOrDigit) {
      sb.append(ch.toChar)
      ch = read()
    }
    if(ch != -1) {
      r.unread(ch)
    }
    sb.toString()
  }
  
  private def read(): Int = {
    while(true) {
      val c = r.read()
      if(c == -1) {
        return -1
      }
      c.toChar match {
        case '\n' => line += 1; column = 0
        case ch if ch.isWhitespace => column += 1
        case ch => column += 1; return ch
      }
    }
    throw new AssertionError()
  }
}
