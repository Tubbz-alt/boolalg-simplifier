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
  
  def parseExpression(ctx: SimplificationContext): Node = parseExpression(ctx, 0)
  
  //returns null on eof
  def parseExpression(ctx: SimplificationContext, precedence: Int): Node = {
    var t = lexer.next()
    if(t.kind == TokenKind.Eof) return null
    val prefix = prefixParselets.getOrElse(t.kind, null)
    if(prefix == null) throw new IllegalArgumentException(s"No prefix parselet for ${t.kind}")
    
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
    if(throwOnEof && a == TokenKind.Eof) throw new IllegalStateException("unexpected eof")
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
    t.expect(kind)
    t
  }
  
  def register(kind: TokenKind, parselet: PrefixParselet): Unit = {
    prefixParselets(kind) = parselet
  }
  
  def register(kind: TokenKind, parselet: InfixParselet): Unit = {
    infixParselets(kind) = parselet
  }
}
