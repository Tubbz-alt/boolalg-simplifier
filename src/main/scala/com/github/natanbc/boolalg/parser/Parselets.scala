package com.github.natanbc.boolalg.parser
import com.github.natanbc.boolalg.{ AndNode, Node, OrNode, SimplificationContext }

object Parselets {
  val Value: PrefixParselet = (ctx, _, token) => ctx.constant(token.value.toInt)
  val Input: PrefixParselet = (ctx, _, token) => ctx.input(token.value)
  val Neg: PrefixParselet = (ctx, p, _) => ctx.neg(p.parseExpression(ctx, Precedence.NEG))
  val LeftParen: PrefixParselet = (ctx, p, _) => {
    val n = p.parseExpression(ctx)
    p.consume(TokenKind.RightParen)
    n
  }
  
  val Or: InfixParselet = new InfixParselet {
    override def parse(ctx: SimplificationContext, parser: Parser, left: Node, token: Token): Node = {
      val flatten = parser.peek().kind != TokenKind.LeftParen
      val parsed = parser.parseExpression(ctx, Precedence.OR)
      val n = parsed match {
        case OrNode(values) if flatten => values
        case v => Seq(v)
      }
      ctx.or((left match {
        case OrNode(values) => values
        case v => Seq(v)
      }) ++ n)
    }
    
    override def precedence: Int = Precedence.OR
  }
  
  val And: InfixParselet = new InfixParselet {
    override def parse(ctx: SimplificationContext, parser: Parser, left: Node, token: Token): Node = {
      val flatten = parser.peek().kind != TokenKind.LeftParen
      val n = parser.parseExpression(ctx, Precedence.AND) match {
        case AndNode(values) if flatten => values
        case v => Seq(v)
      }
      ctx.and((left match {
        case AndNode(values) => values
        case v => Seq(v)
      }) ++ n)
    }
    
    override def precedence: Int = Precedence.AND
  }
}
