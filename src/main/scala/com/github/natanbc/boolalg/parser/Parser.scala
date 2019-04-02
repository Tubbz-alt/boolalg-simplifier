package com.github.natanbc.boolalg.parser

import java.io.{ PushbackReader, StringReader }

import com.github.natanbc.boolalg.{ Node, SimplificationContext }

import scala.collection.mutable

class Parser(source: String) {
  val lexer = new Lexer(new PushbackReader(new StringReader(source)))
  private val prefixParselets = new mutable.HashMap[TokenKind, PrefixParselet]()
  private val infixParselets = new mutable.HashMap[TokenKind, InfixParselet]()
  
  register(TokenKind.Value, Parselets.Value)
  register(TokenKind.Input, Parselets.Input)
  register(TokenKind.Neg, Parselets.Neg)
  register(TokenKind.LeftParen, Parselets.LeftParen)
  register(TokenKind.Or, Parselets.Or)
  register(TokenKind.And, Parselets.And)
  
  def parseExpression(ctx: SimplificationContext): Node = {
    val v = parseExpression(ctx, 0)
    consume(TokenKind.Eof)
    v
  }
  
  def parseExpression(ctx: SimplificationContext, precedence: Int): Node = {
    var t = lexer.next()
    if(t.kind == TokenKind.Eof) {
      throw new IllegalArgumentException(s"Expression expected, got EOF\n${lexer.prettyContext(lexer.pos)}")
    }
    val prefix = prefixParselets.getOrElse(t.kind, null)
    if(prefix == null) {
      throw new IllegalArgumentException(s"No prefix parselet for ${t.kind}\n${lexer.prettyContext(lexer.pos)}")
    }
    
    var left = prefix.parse(ctx, this, t)
    
    while(precedence < currentPrecedence) {
      t = lexer.next()
      val infix = infixParselets(t.kind)
      left = infix.parse(ctx, this, left, t)
    }
    
    left
  }
  
  @inline private def currentPrecedence: Int = {
    val t = peek()
    val parser = infixParselets.getOrElse(t.kind, null)
    if(parser != null) return parser.precedence
    0
  }
  
  def matches(kind: TokenKind, throwOnEof: Boolean = true): Boolean = {
    val a = peek().kind
    if(throwOnEof && a == TokenKind.Eof) {
      throw new IllegalStateException(s"Unexpected EOF\n${lexer.prettyContext(lexer.pos)}")
    }
    val v = a == kind
    if(v) lexer.next()
    v
  }
  
  def peek(): Token = {
    val t = lexer.next()
    lexer.push(t)
    t
  }
  
  def consume(kind: TokenKind): Token = {
    val t = lexer.next()
    if(kind != t.kind) {
      throw new IllegalArgumentException(s"Expected token of type $kind, got ${t.kind} (${t.value}) " +
        s"at line ${t.pos._1}, column ${t.pos._2}\n${lexer.prettyContext(t.pos)}")
    }
    t
  }
  
  def register(kind: TokenKind, parselet: PrefixParselet): Unit = {
    prefixParselets(kind) = parselet
  }
  
  def register(kind: TokenKind, parselet: InfixParselet): Unit = {
    infixParselets(kind) = parselet
  }
}
